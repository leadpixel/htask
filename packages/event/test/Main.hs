module Main
  ( main
  ) where

import Test.Tasty
import EventTests


main :: IO ()
main = defaultMain allTests
