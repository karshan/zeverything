{-# LANGUAGE RankNTypes #-}

module Data.Generics.Validation
  (
  -- * Types 
    TaskList
  , Task (..)
  , Visit
  , Collect
  -- * Core functions
  , zeverything
  , runTask
  -- * Basic Visit and Collect functions
  , preorder
  , breadthfirst
  , collectList
  ) where

import Control.Applicative
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Function

import Data.Generics
import Data.Generics.Zipper

-- Types

-- | A list of tasks running in a functor of type @f@ on zippers with root type 
-- @r@ producing results typed @a@.
type TaskList f r a = [Task f r a]

-- | A single traversal Task.
data Task f r a = Task
  { getZipper :: Zipper r -- ^ Current position
  , getFunction :: Visit f r a -- ^ Function to apply to @zipper@ 
  }
  
-- | Function to visit a zipper and complete a single task.
type Visit f r a = Zipper r -- ^ The zipper to visit
  -> TaskList f r a -- ^ List of remaining tasks
  -> f (a, TaskList f r a) -- ^ Result and updated list of remaining tasks

-- | Function to collect results.
type Collect f a c = 
  Compose Maybe (Compose f ((,) a)) c  -- ^ Result and collection in previous state or nothing
  -> c -- ^ updated or initialized collection

-- Core functions

-- | Traverse a zipper and collect the results.
zeverything :: (Functor f, Data r) =>
  Collect f a c -- ^ Function used to collect results
  -> Visit f r a -- ^ Initial visitation function
  -> Zipper r -- ^ Position to start with
  -> c -- ^ Collection of results
zeverything cf tf z =
 fix (\ f -> cf . fmap f . runTask) $ [(Task z tf)]

---- Internal functions

-- | Run the first task and return its results.
runTask :: (Functor f, Data r) =>
  TaskList f r a -- ^ Available tasks
  -> Compose Maybe (Compose f ((,) a)) (TaskList f r a) -- ^ Result and new list of tasks
runTask ts = 
  case ts of
    [] -> Compose Nothing
    ((Task z f):ts') -> Compose . Just . Compose $ f z ts'

-- Basic Visit and Collect functions

-- | Perform a single preorder (top-down, left-right) search step.
-- Stop descending the current path if the supplied query is successful.
-- Attach the current position to the query result.
-- Can use any applicative functor @f@.
preorder :: (Applicative f, Data r) => 
  GenericQ (Maybe a) -- ^ Query to be applied
  -> Visit f r (Maybe (a, Zipper r)) -- ^ Step result
preorder q z zs = pure $
  case query q z of
    Nothing -> 
      (Nothing, maybeCons (down' z) (preorder q) . maybeCons (right z) (preorder q) $ zs)
    (Just res) -> (Just (res, z), maybeCons (right z) (preorder q) zs)
  where
    maybeCons z f l = case z of 
      (Just x) -> (Task x f) : l
      Nothing -> l

-- | Perform a single breadthfirst (left-right, top-down) search step.
-- Stop descending the current path if the supplied query is successful.
-- Attach the current position to the query result.
-- Can use any applicative functor @f@.
breadthfirst :: (Applicative f, Data a) => 
  GenericQ (Maybe r) -- ^ Query to be applied
  -> Visit f a (Maybe (r, Zipper a)) -- ^ Step result
breadthfirst q z zs = pure $
  case query q z of
    Nothing -> 
      (Nothing, maybeAppend (down' z) (breadthfirst q) . maybeAppend (right z) (breadthfirst q) $ zs)
    (Just res) -> (Just (res, z), maybeAppend (right z) (breadthfirst q) zs)
  where
    maybeAppend z f l = case z of
      (Just x) -> l ++ [Task x f]
      Nothing -> l

-- | Collect results in a list.
-- Uses the identity functor.
collectList :: Collect Identity (Maybe a) [a]
collectList s = case getCompose s of
  Nothing -> []
  (Just x) -> 
    case (runIdentity . getCompose) x of
      (Just res, rs) -> (res:rs)
      (Nothing, rs) -> rs

