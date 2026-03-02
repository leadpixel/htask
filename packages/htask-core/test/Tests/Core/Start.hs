{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.Start (testStart) where

import qualified Data.List        as List
import qualified Data.UUID        as UUID
import qualified Data.UUID.V4     as UUID
import qualified HTask.Core       as H

import           Control.Monad    (void)
import           Data.Time        (Day (ModifiedJulianDay), UTCTime (..))

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0

testStart :: TestTree
testStart = testGroup "start"
  [ testCase "reports success when starting a task" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        H.startTask (H.taskUuidToText taskId)

      let result = getResult output
      isSuccess result @? "starts a pending task"


  , testCase "marks the task as in-progress" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        void $ H.startTask (H.taskUuidToText taskId)
        tasks <- H.listTasks
        pure (taskId, tasks)

      let (taskId, tasks) = getResult output
      Just H.InProgress @=? (H.status <$> List.find (\x -> H.taskUuid x == taskId) tasks)


  , testCase "fails if there is no matching task" $ do
      uuid <- UUID.nextRandom
      output <- runTestApp fakeTime $ H.startTask (UUID.toText uuid)
      let result = getResult output
      H.FailedToFind @=? result

  , testCase "fails when task is complete" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        void $ H.startTask (H.taskUuidToText taskId)
        void $ H.completeTask (H.taskUuidToText taskId)
        H.startTask (H.taskUuidToText taskId)

      let result = getResult output
      H.FailedToModify @=? result
  ]

  where
    isSuccess (H.ModifySuccess _) = True
    isSuccess _                   = False
