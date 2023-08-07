module Main (main) where

import qualified Tests.Core.Add      as APITest
import qualified Tests.Core.Complete as APITest
import qualified Tests.Core.List     as APITest
import qualified Tests.Core.Remove   as APITest
import qualified Tests.Core.Start    as APITest
import qualified Tests.Core.Stop     as APITest

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
