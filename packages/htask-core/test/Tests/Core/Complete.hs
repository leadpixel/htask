{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.Complete (testComplete) where

import qualified Data.List        as List
import qualified HTask.Core       as H

import           Control.Monad    (void)
import           Data.Time        (Day (ModifiedJulianDay), UTCTime (..))

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testComplete :: TestTree
testComplete = testGroup "complete"
  [ testCase "responds with success" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId)  <- H.addTask "some task"
        H.completeTask (H.taskUuidToText taskId)

      let result = getResult output
      isSuccess result @? "expected success"


  , testCase "marks the task as complete" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId)  <- H.addTask "some task"
        void $ H.completeTask (H.taskUuidToText taskId)
        tasks <- H.listTasks
        pure (taskId, tasks)

      let (taskId, tasks) = getResult output
      Just H.Complete @=? (H.status <$> List.find (\x -> H.taskUuid x == taskId) tasks)


  , testCase "fails when unable to find a matching task" $ do
      output <- runTestApp fakeTime $ do
        void $ H.addTask "some task"
        H.completeTask "unknown"

      let result = getResult output
      H.FailedToFind @=? result

  ]

  where
    isSuccess (H.ModifySuccess _) = True
    isSuccess _                   = False
