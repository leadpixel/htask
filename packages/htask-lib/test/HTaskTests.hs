module HTaskTests
  ( allTests
  ) where

import Test.Tasty
import HTaskTests.Tasks
import HTaskTests.Log


allTests :: TestTree
allTests = testGroup "htask tests"
  [ test_tasks
  , test_log
  ]
