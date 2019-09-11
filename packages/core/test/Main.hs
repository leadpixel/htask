module Main
  ( main
  ) where

import qualified APITests.Add      as APITest
import qualified APITests.Complete as APITest
import qualified APITests.List     as APITest
import qualified APITests.Remove   as APITest
import qualified APITests.Start    as APITest
import qualified APITests.Stop     as APITest

import           Test.Tasty


main :: IO ()
main = defaultMain allTests


allTests :: TestTree
allTests = testGroup "htask"
  [ APITest.testAdd
  , APITest.testComplete
  , APITest.testList
  , APITest.testRemove
  , APITest.testStart
  , APITest.testStop
  ]
