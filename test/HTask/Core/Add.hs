{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Core.Add (testAdd) where

import           Data.Functor       (($>))
import qualified HTask.Core         as Core

import           Data.Time          (Day (ModifiedJulianDay), UTCTime (..))

import           HTask.Core.TestApp
import           Test.Tasty
import           Test.Tasty.HUnit

fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0

testAdd :: TestTree
testAdd = testGroup "add"
  [ testCase "returns the created uuid on success" $ do
      output <- runTestApp fakeTime $ Core.addTask "some task"
      let result = getResult output
      isSuccess result @? "expected success"

  , testCase "tasks start out pending" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId) <- Core.addTask "some task"
        tasks <- Core.listTasks
        pure (taskId, tasks)

      let (taskId, tasks) = getResult output
      let tsk = head tasks
      taskId @=? Core.taskUuid tsk
      Core.Pending @=? Core.status tsk

  , testCase "creates a unique id for each task" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess id1) <- Core.addTask "task 1"
        (Core.AddSuccess id2) <- Core.addTask "task 2"
        pure (id1, id2)

      let (id1, id2) = getResult output
      (id1 /= id2) @? "expected ids to be different"

  , testCase "ignores duplicates" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId) <- Core.addTask "some task"
        void $ Core.addTask "some task"
        tasks <- Core.listTasks
        pure (taskId, tasks)

      let (_taskId, tasks) = getResult output
      1 @=? length tasks

  , testCase "disallows empty description" $ do
      output <- runTestApp fakeTime $ Core.addTask ""
      let result = getResult output
      isEmptyDescription result @? "expected EmptyDescription"
  ]

  where
    isSuccess (Core.AddSuccess _) = True
    isSuccess _                   = False

    isEmptyDescription Core.EmptyDescription = True
    isEmptyDescription _                     = False

    void :: (Monad m) => m a -> m ()
    void op = op $> ()
