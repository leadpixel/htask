module Main
  ( main
  ) where

import           HTaskTests
import           Test.Tasty


main :: IO ()
main = defaultMain allTests
