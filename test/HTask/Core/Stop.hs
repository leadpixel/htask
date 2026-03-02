{-# LANGUAGE OverloadedStrings #-}

module HTask.Core.Stop (testStop) where

import qualified Data.List          as List
import qualified HTask.Core         as H

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
        (H.AddSuccess taskId) <- H.addTask "some task"
        void $ H.startTask (H.taskUuidToText taskId)
        H.stopTask (H.taskUuidToText taskId)

      let result = getResult output
      isSuccess result @? "expected success"


  , testCase "marks the task as pending" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        void $ H.startTask (H.taskUuidToText taskId)
        void $ H.stopTask (H.taskUuidToText taskId)
        tasks <- H.listTasks
        pure (taskId, tasks)

      let (taskId, tasks) = getResult output
      Just H.Pending @=? (H.status <$> List.find (\x -> H.taskUuid x == taskId) tasks)


  , testCase "fails when task is already pending" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        H.stopTask (H.taskUuidToText taskId)

      let result = getResult output
      H.FailedToModify @=? result

  , testCase "fails when task is complete" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        void $ H.startTask (H.taskUuidToText taskId)
        void $ H.completeTask (H.taskUuidToText taskId)
        H.stopTask (H.taskUuidToText taskId)

      let result = getResult output
      H.FailedToModify @=? result
  ]

  where
    isSuccess (H.ModifySuccess _) = True
    isSuccess _                   = False
