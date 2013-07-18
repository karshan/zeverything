{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, RankNTypes #-}
module Data.Generics.Validation.Test
  ( properties
  ) where

import Data.Generics.Validation

import Test.QuickCheck
import Test.QuickCheck.All

import Data.Generics
import Data.Generics.Zipper

import Data.Functor.Compose
import Data.Functor.Identity
import Control.Monad
import Data.Maybe
import Data.List (unfoldr)

properties = $(forAllProperties)

-- | Check that runTask terminates for empty lists.
prop_emptyTaskList = isNothing . getCompose $ runTask emptyList
  where
    emptyList :: TaskList Identity Int Int
    emptyList = []

-- | Return a dummy task to fill task lists.
dummyTask :: Task f r a
dummyTask = error "This task may not be used"

-- | Check that runTask uses the first elements visit function.
prop_runTaskStep :: 
  ( Arbitrary a, Data a, Show a, Eq a) => a -> Bool
prop_runTaskStep td =
  case getCompose $ runTask [(Task (toZipper td) visit), dummyTask] of
    Just x -> 
      case (runIdentity . getCompose $ x) of
        (res, []) -> res == td
        _ -> False
    _ -> False
  where
    visit z ts = Identity (fromZipper z, []) 

-- | Checks that collectList creates a new List.
prop_collectListInitial =
  null . collectList $ nothingInt
  where
    nothingInt :: Compose Maybe (Compose Identity ((,) (Maybe Int))) [Int]
    nothingInt = Compose Nothing

-- | Checks that collectList filters Nothing values and updates the result list.
prop_collectListStep x xs = 
  case x of
    (Just i) ->  (i:xs) == (collectList $ wrapped x xs)
    Nothing -> xs == (collectList $ wrapped x xs)
  where
    wrapped x xs = Compose . Just . Compose $ Identity (x, xs)

-- | Obtain a Zipper at an arbitrary position inside an arbitrary data structure.
instance (Arbitrary a, Data a) => Arbitrary (Zipper a) where
  arbitrary = do
    d <- arbitrary
    dirs <- listOf (elements [down', left, right, up])
    return $ foldl tryToMove (toZipper d) dirs
    where
      tryToMove pos dir = case dir pos of
        Nothing -> pos
        (Just pos') -> pos'

-- | Show an arbitrary Zipper by showing its hole
instance (Show a) => Show (Zipper a) where
  show = query gshow

-- | Nested test data structure
data TestData a b c =
  Void -- ^ No content
  | Simple a -- ^ Just a single element of type a
  | Product a b -- ^ Product of two elements
  | Triple a b c -- ^ Triple of three elements
  | Four a b c a -- ^ Four elements
  | List a (TestData a b c) -- ^ List of an a and TestData
  | HaskellTuple (a, b) -- ^ Haskell version of products
  | HaskellList [a] -- ^ Haskell version of lists
  deriving (Eq, Data, Typeable, Show)

-- | Construct arbitrary TestData
instance (Arbitrary a, Arbitrary b, Arbitrary c) => 
  Arbitrary (TestData a b c) where
  arbitrary = 
    oneof [ return Void
          , liftM Simple arbitrary
          , liftM2 Product arbitrary arbitrary
          , liftM3 Triple arbitrary arbitrary arbitrary
          , liftM4 Four arbitrary arbitrary arbitrary arbitrary
          , liftM2 List arbitrary arbitrary
          , liftM HaskellTuple arbitrary
          , liftM HaskellList arbitrary
          ]

type IntTestData = TestData (TestData Int Int Int) Int Int
type SelectFunction = 
  [Task Identity IntTestData (Maybe (Int, Zipper IntTestData))] 
  -> Task Identity IntTestData (Maybe (Int, Zipper IntTestData))

-- | Check a single search step of a preorder or breadthfirst search.
_prop_searchStep ::
  (GenericQ (Maybe Int) -> Visit Identity IntTestData (Maybe (Int, Zipper IntTestData))) -- ^ Search function
  -> SelectFunction -- ^ Selects the task for down from a list
  -> SelectFunction -- ^ Selects the task for right from a list
  -> SelectFunction -- ^ Selects the task for down or right from an incomplete list
  -> Maybe Int -- ^ Static simulated query result
  -> Zipper (TestData (TestData Int Int Int) Int Int) -- ^ Zipper at test position
  -> Bool
_prop_searchStep search downTask rightTask downOrRightTask qres zipper =
  case (qres, search (const qres) zipper [dummyTask]) of
    (Nothing, (Identity (Nothing, ts@[t1, t2, t3]))) -> 
      (isJust $ down' zipper) && check up (downTask ts) 
      && (isJust $ right zipper) && check left (rightTask ts)
    (Nothing, (Identity (Nothing, ts@[t1, t2]))) -> 
      (check up (downOrRightTask ts) && (isNothing $ right zipper)) 
      || (check left (downOrRightTask ts) && (isNothing $ down' zipper))
    (Nothing, (Identity (Nothing, [dt]))) -> 
      (isNothing $ right zipper) && (isNothing $ down' zipper)
    (Just res, (Identity ((Just (res', z)), ts@[t1, t2]))) -> 
      res == res' && checkHoles z zipper 
      && check left (downOrRightTask ts) && (isJust $ right zipper)
    (Just res, (Identity ((Just (res', z)), [dt]))) ->
      res == res' && checkHoles z zipper && (isNothing $ right zipper)
    _ -> False
  where
    check back t = 
      case (back . getZipper) t of
        (Just x) -> checkHoles x zipper
        Nothing -> False
    checkHoles z1 z2 =
      (getHoleInt z1 == getHoleInt z2) 
      && (getHoleTestDataInner z1 == getHoleTestDataInner z2)
      && (getHoleIntTestData z1 == getHoleIntTestData z2)
    getHoleInt :: Zipper a -> Maybe Int
    getHoleInt = getHole
    getHoleTestDataInner :: Zipper a -> Maybe (TestData Int Int Int)
    getHoleTestDataInner = getHole
    getHoleIntTestData :: Zipper a -> Maybe (IntTestData)
    getHoleIntTestData = getHole

-- | Checks a single preorder step on a given data structure with a given
-- query result.
prop_preorderStep :: Maybe Int -> Zipper (TestData (TestData Int Int Int) Int Int) -> Bool
prop_preorderStep = 
  _prop_searchStep preorder head (head . tail) head

-- | Checks a single breadthfirst step on a given data structure with a given
-- query result.
prop_breadthfirstStep :: Maybe Int -> Zipper (TestData (TestData Int Int Int) Int Int) -> Bool
prop_breadthfirstStep =
  _prop_searchStep breadthfirst last (head . tail . reverse) (last)

-- | Manual preorder search of Int TestData
toPreorder :: TestData a b a -> [a]
toPreorder (List x y) = (x:(toPreorder y))
toPreorder (Void) = []
toPreorder (Simple x) = [x]
toPreorder (Product x y) = [x]
toPreorder (Triple x y z) = [x, z]
toPreorder (Four x y z x2) = [x, z, x2]
toPreorder (HaskellTuple (x, y)) = [x]
toPreorder (HaskellList xs) = xs

-- | Manual breadthfirst search of Int TestData
toBreadthFirst :: TestData a b a -> [a]
toBreadthFirst x = unfoldr bfsStep [x]
  where
    bfsStep toDo = case toDo of
      ((List x y):xs) -> Just (x, xs ++ [y])
      ((Void):xs) -> bfsStep xs
      ((Simple x):xs) -> Just (x, xs)
      ((Product x y):xs) -> Just (x, xs)
      ((Triple x y z):xs) -> Just (x, xs++[(Simple z)])
      ((Four x y z x2):xs) -> Just (x, xs++[(Triple z y x2)])
      ((HaskellTuple (x, y)):xs) -> Just (x, xs)
      ((HaskellList []):xs) -> bfsStep xs
      ((HaskellList [x]):xs) -> Just (x, xs)
      ((HaskellList (x:ys)):xs) -> bfsStep (xs++[(Simple x), (HaskellList ys)])
      [] -> Nothing

_prop_manualSearch ::
  (TestData Int Char Int -> [Int]) -- ^ Handwritten search function
  -> (GenericQ (Maybe Int) -> Visit Identity (TestData Int Char Int) (Maybe (Int, Zipper (TestData Int Char Int)))) -- ^ Search function
  -> (TestData Int Char Int) -- ^ Test data structure
  -> Bool
_prop_manualSearch manualSearch search td = 
  manualSearch td == (map fst $ zeverything collectList (search q) (toZipper td))
  where
    q :: GenericQ (Maybe Int)
    q = mkQ Nothing Just

-- | Checks if zeverything collectList preorder is the same as manual
-- preorder on TestData Int Int Int
prop_manualPreorder :: TestData Int Char Int -> Bool
prop_manualPreorder = 
  _prop_manualSearch toPreorder preorder

-- | Checks if zeverything collectList breadthfirst is the same as manual
-- breadthfirst on TestData Int Int Int
prop_manualBreadthfirst :: TestData Int Char Int -> Bool
prop_manualBreadthfirst = 
  _prop_manualSearch toBreadthFirst breadthfirst

