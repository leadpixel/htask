{-# LANGUAGE OverloadedStrings #-}

module HTask.Core.Stop (testStop) where

import qualified Data.List          as List
import qualified HTask.Core         as Core

import           Control.Monad      (void)
import           Data.Time          (Day (ModifiedJulianDay), UTCTime (..))

import           HTask.Core.TestApp
import           Test.Tasty
import           Test.Tasty.HUnit

fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0

testStop :: TestTree
testStop = testGroup "stop"
  [ testCase "reports success when stopping a task" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId) <- Core.addTask "some task"
        void $ Core.startTask (Core.taskUuidToText taskId)
        Core.stopTask (Core.taskUuidToText taskId)

      let result = getResult output
      isSuccess result @? "expected success"

  , testCase "marks the task as pending" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId) <- Core.addTask "some task"
        void $ Core.startTask (Core.taskUuidToText taskId)
        void $ Core.stopTask (Core.taskUuidToText taskId)
        tasks <- Core.listTasks
        pure (taskId, tasks)

      let (taskId, tasks) = getResult output
      Just Core.Pending @=? (Core.status <$> List.find (\x -> Core.taskUuid x == taskId) tasks)

  , testCase "fails when task is already pending" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId) <- Core.addTask "some task"
        Core.stopTask (Core.taskUuidToText taskId)

      let result = getResult output
      Core.FailedToModify @=? result

  , testCase "fails when task is complete" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId) <- Core.addTask "some task"
        void $ Core.startTask (Core.taskUuidToText taskId)
        void $ Core.completeTask (Core.taskUuidToText taskId)
        Core.stopTask (Core.taskUuidToText taskId)

      let result = getResult output
      Core.FailedToModify @=? result
  ]

  where
    isSuccess (Core.ModifySuccess _) = True
    isSuccess _                      = False
