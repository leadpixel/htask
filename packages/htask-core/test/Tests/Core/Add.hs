{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Core.Add (testAdd) where

import qualified Data.Map         as Map
import qualified Data.Sequence    as Seq
import qualified Data.Time        as Time
import qualified Data.UUID.V4     as UUID
import qualified HTask.Core       as H
import qualified Leadpixel.Events as V

import           Control.Monad    (void)
import           Data.Tagged      (Tagged (..))
import           Data.Time        (UTCTime (..))

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = UTCTime (Time.ModifiedJulianDay 0) 0


testAdd :: TestTree
testAdd = testGroup "add"
  [  returnsCreatedUuid
  , storesCreatedTask
  -- , rollsBackOnWriteFailure
  , writesEvent
  , doesNotAllowDuplicateUuids
  , doesNotStoreDuplicateTask
  , doesNotWriteFailedEvent
  ]


returnsCreatedUuid :: TestTree
returnsCreatedUuid = testCase "returns the created uuid on success" $ do
  uuid <- UUID.nextRandom
  let op = H.addTask "some task"
  result <- getResult <$> runTestApp (uuid, fakeTime) op
  assertEqual "expecting success" (H.AddSuccess (Tagged uuid)) result


storesCreatedTask :: TestTree
storesCreatedTask = testCase "stores the created task" $ do
  uuid <- UUID.nextRandom
  tasks <- fmap getTasks $ runTestApp (uuid, fakeTime) $ H.addTask "some task"

  assertEqual "expecting one task" 1 (Map.size tasks)

  let expectedTask =  H.Task
        { H.taskUuid = Tagged uuid
        , H.description = "some task"
        , H.createdAt = fakeTime
        , H.status = H.Pending
        }
  assertEqual "task status" (Just expectedTask) (Map.lookup (Tagged uuid :: H.TaskUuid) tasks)


writesEvent :: TestTree
writesEvent = testCase "stores one event" $ do
  uuid <- UUID.nextRandom
  events <- fmap getEvents $ runTestApp (uuid, fakeTime) $ H.addTask "some task"

  let expectedEvents = Seq.fromList [ V.Event { V.timestamp = fakeTime, V.payload = H.AddTask (Tagged uuid) "some task" } ]
  assertEqual "events count" 1 (Seq.length events)
  assertEqual "events" expectedEvents events

  let intent = H.AddTask (Tagged uuid) "some task"
  let ev = V.Event { V.timestamp = fakeTime, V.payload = intent}
  assertEqual "events" (Just ev) (Seq.lookup 0 events)


-- rollsBackOnWriteFailure :: TestTree
-- rollsBackOnWriteFailure = testCase "does not store task on write failure" $ do
--   uuid <- UUID.nextRandom
--   (_, x) <- runWriteFailure (uuid, fakeTime) (H.addTask "other task")
--   assertEqual "expecting nothing" [] x


doesNotAllowDuplicateUuids :: TestTree
doesNotAllowDuplicateUuids = testCase "cannot use a non-unique id" $ do
  uuid <- UUID.nextRandom
  result <- fmap getResult $ runTestApp (uuid, fakeTime) $ do
    void $ H.addTask "some task"
    H.addTask "some task"

  assertEqual "expecting failure" H.FailedToAdd result


doesNotStoreDuplicateTask :: TestTree
doesNotStoreDuplicateTask = testCase "only stores the original task" $ do
  uuid <- UUID.nextRandom
  x <- getTasks <$> runTestApp (uuid, fakeTime) (H.addTask "some task" >> H.addTask "other task")
  assertEqual "expecting one task"
    (Map.singleton (Tagged uuid) $ H.Task
      { H.taskUuid = Tagged uuid
      , H.description = "some task"
      , H.createdAt = fakeTime
      , H.status = H.Pending
      }) x


doesNotWriteFailedEvent :: TestTree
doesNotWriteFailedEvent = testCase "only stores one event" $ do
  uuid <- UUID.nextRandom
  x <- getEvents <$> runTestApp (uuid, fakeTime) (H.addTask "some task" >> H.addTask "other task")

  let expectedEvents = Seq.fromList [ H.AddTask (Tagged uuid) "some task" ]
  assertEqual "expecting one 'add-task' intent" expectedEvents (V.payload <$> x)
