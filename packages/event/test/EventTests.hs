module EventTests
  ( allTests
  ) where

import Test.Tasty
import EventTests.Tasks
import EventTests.Log


allTests :: TestTree
allTests = testGroup "htask tests"
  [ test_tasks
  , test_log
  ]
