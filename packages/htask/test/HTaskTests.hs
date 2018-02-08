{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module HTaskTests
  ( allTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty.HUnit

import Lib
import API


allTests :: TestTree
allTests = testGroup "htask tests"
  [ test_tasks
  , test_log
  , test_rebuild
  ]


test_tasks :: TestTree
test_tasks = testGroup "tasks"
  [ listingEmptyTasks
  , addingATask
  ]

  where
    listingEmptyTasks :: TestTree
    listingEmptyTasks = testCase "listing empty tasks" $ do
      (ts, _log) <- runTaskApi (listTasks)
      assertEqual "expecting no tasks" 0 (length ts)

    addingATask :: TestTree
    addingATask = testCase "adding one task" $ do
      (ts, _log) <- runTaskApi (addTask "some task" >> listTasks)
      assertEqual "expecting one task" 1 (length ts)



test_log :: TestTree
test_log = testGroup "logs"
  [ listingEmptyTasks
  , addingATask
  ]

  where
    listingEmptyTasks :: TestTree
    listingEmptyTasks = testCase "listing empty tasks" $ do
      (_ts, log) <- runTaskApi (listTasks)
      assertEqual "expecting no logs" 0 (length log)

    addingATask :: TestTree
    addingATask = testCase "adding one task" $ do
      (_ts, log) <- runTaskApi (addTask "some task")
      assertEqual "expecting one log entry" 1 (length log)



deriving instance Eq Task

test_rebuild :: TestTree
test_rebuild = testProperty "can rebuild task state from log" thing
  where
    thing :: Property
    thing = forAll genTaskInteractions $ \p ->
      monadicIO $ run $ do
        (a, ls) <- runTaskApi (p >> listTasks)
        let b = rebuildFromLog ls
        assertEqual "rebuild should be identical" a b


genTaskInteractions :: Gen (TaskMonad a)
genTaskInteractions = undefined


rebuildFromLog :: EventLog -> Tasks
rebuildFromLog = undefined
