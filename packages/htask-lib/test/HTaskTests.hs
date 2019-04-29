module HTaskTests
  ( allTests
  ) where

import           HTaskTests.Log
import           HTaskTests.Tasks
import           Test.Tasty


allTests :: TestTree
allTests = testGroup "htask tests"
  [ test_tasks
  , test_log
  ]
