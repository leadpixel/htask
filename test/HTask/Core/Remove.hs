{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Core.Remove (testRemove) where

import qualified Data.List          as List
import qualified Data.UUID          as UUID
import qualified Data.UUID.V4       as UUID
import qualified HTask.Core         as Core

import           Control.Monad      (void)
import           Data.Time          (Day (ModifiedJulianDay), UTCTime (..))

import           HTask.Core.TestApp
import           Test.Tasty
import           Test.Tasty.HUnit


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testRemove :: TestTree
testRemove = testGroup "remove"
  [ testCase "reports success when removing a task" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId) <- Core.addTask "some task"
        Core.removeTask (Core.taskUuidToText taskId)

      let result = getResult output
      isSuccess result @? "expected success"


  , testCase "marks the task as abandoned" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId) <- Core.addTask "some task"
        void $ Core.removeTask (Core.taskUuidToText taskId)
        tasks <- Core.listTasks
        pure (taskId, tasks)

      let (taskId, tasks) = getResult output
      Just Core.Abandoned @=? (Core.status <$> List.find (\x -> Core.taskUuid x == taskId) tasks)


  , testCase "fails if there is no matching task" $ do
      uuid <- UUID.nextRandom
      output <- runTestApp fakeTime $ Core.removeTask (UUID.toText uuid)
      let result = getResult output
      Core.FailedToFind @=? result

  , testCase "fails when task is complete" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId) <- Core.addTask "some task"
        void $ Core.startTask (Core.taskUuidToText taskId)
        void $ Core.completeTask (Core.taskUuidToText taskId)
        Core.removeTask (Core.taskUuidToText taskId)

      let result = getResult output
      Core.FailedToModify @=? result
  ]


  where
    isSuccess (Core.ModifySuccess _) = True
    isSuccess _                      = False
