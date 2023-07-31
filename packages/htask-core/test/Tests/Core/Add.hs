{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.Add
  ( testAdd
  ) where

import qualified Data.Time                 as Time
import qualified Data.UUID.V4              as UUID
import qualified HTask.Core                as H
import qualified Leadpixel.Events          as V

import           Data.Tagged               (Tagged (..))
import           Data.Time                 (UTCTime (..))

import           Test.QuickCheck.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = UTCTime (Time.ModifiedJulianDay 0) 0


testAdd :: TestTree
testAdd = testGroup "add"
  [ testSuccess
  , testFailure
  ]

  where
    testSuccess = testGroup "success"
      [ returnsCreatedUuid
      , storesCreatedTask
      -- , rollsBackOnWriteFailure
      , writesEvent
      ]

    testFailure = testGroup "failure"
      [ doesNotAllowDuplicateUuids
      , doesNotStoreDuplicateTask
      , doesNotWriteFailedEvent
      ]


returnsCreatedUuid :: TestTree
returnsCreatedUuid = testCase "returns the created uuid on success" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) (H.addTask "some task")
  assertEqual "expecting success" (H.AddSuccess (Tagged uuid)) x


storesCreatedTask :: TestTree
storesCreatedTask = testCase "stores the created task" $ do
  uuid <- UUID.nextRandom
  x <- runTasks (uuid, fakeTime) (H.addTask "some task")
  assertEqual "expecting one task"
    (pure  H.Task
      { H.taskUuid = Tagged uuid
      , H.description = "some task"
      , H.createdAt = fakeTime
      , H.status = H.Pending
      }
    ) x

-- rollsBackOnWriteFailure :: TestTree
-- rollsBackOnWriteFailure = testCase "does not store task on write failure" $ do
--   uuid <- UUID.nextRandom
--   (_, x) <- runWriteFailure (uuid, fakeTime) (H.addTask "other task")
--   assertEqual "expecting nothing" [] x


writesEvent :: TestTree
writesEvent = testCase "stores one event" $ do
  uuid <- UUID.nextRandom
  x <- runEventLog (uuid, fakeTime) (H.addTask "some task")
  assertEqual "expecting 'add-task' intent" [ H.AddTask "some task" ] (H.intent . V.payload <$> x)


doesNotAllowDuplicateUuids :: TestTree
doesNotAllowDuplicateUuids = testCase "cannot use a non-unique id" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) (H.addTask "some task" >> H.addTask "other task")
  assertEqual "expecting failure" H.FailedToAdd x


doesNotStoreDuplicateTask :: TestTree
doesNotStoreDuplicateTask = testCase "only stores the original task" $ do
  uuid <- UUID.nextRandom
  x <- runTasks (uuid, fakeTime) (H.addTask "some task" >> H.addTask "other task")
  assertEqual "expecting one task"
    (pure H.Task
      { H.taskUuid = Tagged uuid
      , H.description = "some task"
      , H.createdAt = fakeTime
      , H.status = H.Pending
      }) x


doesNotWriteFailedEvent :: TestTree
doesNotWriteFailedEvent = testCase "only stores one event" $ do
  uuid <- UUID.nextRandom
  x <- runEventLog (uuid, fakeTime) (H.addTask "some task" >> H.addTask "other task")
  assertEqual "expecting one 'add-task' intent" [ H.AddTask "some task" ] (H.intent . V.payload <$> x)
