module Main
  ( main
  ) where

import Test.Tasty
import HTaskTests


main :: IO ()
main = defaultMain allTests
