{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Core.Remove (testRemove) where

import qualified Data.List          as List
import qualified Data.UUID          as UUID
import qualified Data.UUID.V4       as UUID
import qualified HTask.Core         as H

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
        (H.AddSuccess taskId) <- H.addTask "some task"
        H.removeTask (H.taskUuidToText taskId)

      let result = getResult output
      isSuccess result @? "expected success"


  , testCase "marks the task as abandoned" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        void $ H.removeTask (H.taskUuidToText taskId)
        tasks <- H.listTasks
        pure (taskId, tasks)

      let (taskId, tasks) = getResult output
      Just H.Abandoned @=? (H.status <$> List.find (\x -> H.taskUuid x == taskId) tasks)


  , testCase "fails if there is no matching task" $ do
      uuid <- UUID.nextRandom
      output <- runTestApp fakeTime $ H.removeTask (UUID.toText uuid)
      let result = getResult output
      H.FailedToFind @=? result

  , testCase "fails when task is complete" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        void $ H.startTask (H.taskUuidToText taskId)
        void $ H.completeTask (H.taskUuidToText taskId)
        H.removeTask (H.taskUuidToText taskId)

      let result = getResult output
      H.FailedToModify @=? result
  ]


  where
    isSuccess (H.ModifySuccess _) = True
    isSuccess _                   = False
