{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.Stop (testStop) where

import qualified Data.Map.Strict  as Map
import qualified Data.UUID        as UUID
import qualified Data.UUID.V4     as UUID
import qualified HTask.Core       as H

import           Control.Monad    (void)
import           Data.Time        (UTCTime)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = read "2023-08-09 11:21:00 UTC"


testStop :: TestTree
testStop = testGroup "stop"
  [ testCase "fails if there is no matching event" $ do
      uuid <- UUID.nextRandom
      output <- runTestApp fakeTime $ H.stopTask (UUID.toText uuid)

      let result = getResult output
      assertEqual "expecting failure" H.FailedToFind result


  , testCase "does not create a task on failure" $ do
      uuid <- UUID.nextRandom
      output <- runTestApp fakeTime $ H.stopTask (UUID.toText uuid)

      let tasks = getTasks output
      assertEqual "expecting no tasks" mempty tasks


  , testCase "reports success when stopping a task" $ do
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
        pure taskId

      let tasks = getTasks output
      let taskId = getResult output

      1 @=? Map.size tasks
      Just H.Pending @=? (H.status <$> Map.lookup taskId tasks)


  , testCase "cannot stop a stopped task" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        void $ H.startTask (H.taskUuidToText taskId)
        void $ H.stopTask (H.taskUuidToText taskId)
        H.stopTask (H.taskUuidToText taskId)

      let result = getResult output
      H.FailedToModify @=? result

  ]

  where
    isSuccess (H.ModifySuccess _) = True
    isSuccess _                   = False
