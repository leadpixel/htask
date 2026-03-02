{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Core.Add (testAdd) where

import qualified HTask.Core         as H

import           Data.Time          (Day (ModifiedJulianDay), UTCTime (..))

import           HTask.Core.TestApp
import           Test.Tasty
import           Test.Tasty.HUnit


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testAdd :: TestTree
testAdd = testGroup "add"
  [ testCase "returns the created uuid on success" $ do
      output <- runTestApp fakeTime $ H.addTask "some task"
      let result = getResult output
      isSuccess result @? "expected success"


  , testCase "tasks start out pending" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        tasks <- H.listTasks
        pure (taskId, tasks)

      let (taskId, tasks) = getResult output
      let tsk = head tasks
      taskId @=? H.taskUuid tsk
      H.Pending @=? H.status tsk


  , testCase "creates a unique id for each task" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess id1) <- H.addTask "task 1"
        (H.AddSuccess id2) <- H.addTask "task 2"
        pure (id1, id2)

      let (id1, id2) = getResult output
      (id1 /= id2) @? "expected ids to be different"


  , testCase "ignores duplicates" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        void $ H.addTask "some task"
        tasks <- H.listTasks
        pure (taskId, tasks)

      let (_taskId, tasks) = getResult output
      1 @=? length tasks

  ]

  where
    isSuccess (H.AddSuccess _) = True
    isSuccess _                = False

    void :: (Monad m) => m a -> m ()
    void op = op $> ()
