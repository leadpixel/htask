{-# LANGUAGE OverloadedStrings #-}

module HTask.Core.Complete (testComplete) where

import qualified Data.List          as List
import qualified HTask.Core         as Core

import           Control.Monad      (void)
import           Data.Time          (Day (ModifiedJulianDay), UTCTime (..))

import           HTask.Core.TestApp
import           Test.Tasty
import           Test.Tasty.HUnit


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testComplete :: TestTree
testComplete = testGroup "complete"
  [ testCase "responds with success" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId)  <- Core.addTask "some task"
        void $ Core.startTask (Core.taskUuidToText taskId)
        Core.completeTask (Core.taskUuidToText taskId)

      let result = getResult output
      isSuccess result @? "expected success"


  , testCase "marks the task as complete" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId)  <- Core.addTask "some task"
        void $ Core.startTask (Core.taskUuidToText taskId)
        void $ Core.completeTask (Core.taskUuidToText taskId)
        tasks <- Core.listTasks
        pure (taskId, tasks)

      let (taskId, tasks) = getResult output
      Just Core.Complete @=? (Core.status <$> List.find (\x -> Core.taskUuid x == taskId) tasks)


  , testCase "fails when unable to find a matching task" $ do
      output <- runTestApp fakeTime $ do
        void $ Core.addTask "some task"
        Core.completeTask "unknown"

      let result = getResult output
      Core.FailedToFind @=? result

  , testCase "fails when task is pending" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId)  <- Core.addTask "some task"
        Core.completeTask (Core.taskUuidToText taskId)

      let result = getResult output
      Core.FailedToModify @=? result
  ]

  where
    isSuccess (Core.ModifySuccess _) = True
    isSuccess _                      = False
