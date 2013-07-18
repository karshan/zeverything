module Main where

import Test.QuickCheck
import qualified Data.Generics.Validation.Test

args = stdArgs 
  { maxSuccess = 999
  , maxSize = 999
  }

main = do
  Data.Generics.Validation.Test.properties (quickCheckWithResult args)

