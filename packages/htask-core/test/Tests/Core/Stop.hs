{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.Stop (testStop) where

import qualified Data.Map.Strict  as Map
import qualified Data.Sequence    as Seq
import qualified Data.UUID        as UUID
import qualified Data.UUID.V4     as UUID
import qualified HTask.Core       as H
import qualified Leadpixel.Events as V

import           Control.Monad    (void)
import           Data.Tagged
import           Data.Time        (UTCTime)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = read "2023-08-09 11:21:00 UTC"


testStop :: TestTree
testStop = testGroup "stop"
  [ cannotStopNonExistentEvent
  , canStopEvent
  , cannotStopStopped
  ]


cannotStopNonExistentEvent :: TestTree
cannotStopNonExistentEvent = testCase "fails if there is no matching event" $ do
  uuid <- UUID.nextRandom
  let op = H.stopTask (UUID.toText uuid)

  (result, tasks, events) <- runTestApp (uuid, fakeTime) op

  assertEqual "expecting failure" H.FailedToFind result
  assertEqual "expecting no tasks" mempty tasks
  assertEqual "expecting no events" mempty events


canStopEvent :: TestTree
canStopEvent = testCase "reports success when stopping a task" $ do
  uuid <- Tagged <$> UUID.nextRandom
  let op = do
        void $ H.addTask "some task"
        void $ H.startTask (UUID.toText $ untag uuid)
        H.stopTask (UUID.toText $ untag uuid)

  (result, tasks, events) <- runTestApp (untag uuid, fakeTime) op

  let expectedResult = H.ModifySuccess $ H.Task
        { H.taskUuid = uuid
        , H.description = "some task"
        , H.createdAt = fakeTime
        , H.status = H.Pending
        }

  assertEqual "puts a task back into pending" expectedResult result

  assertEqual "tasks count" 1 (Map.size tasks)
  assertEqual "task status" (Just H.Pending) (H.status <$> Map.lookup uuid tasks)

  assertEqual "events count" 3 (Seq.length events)

  let intent = H.StopTask uuid
  let ev = V.Event { V.timestamp = fakeTime, V.payload = intent}
  assertEqual "events" (Just ev) (Seq.lookup 2 events)


cannotStopStopped :: TestTree
cannotStopStopped = testCase "cannot stop a stopped task" $ do
  uuid <- Tagged <$> UUID.nextRandom
  let op = do
        void $ H.addTask "some task"
        void $ H.startTask (UUID.toText $ untag uuid)
        void $ H.stopTask (UUID.toText $ untag uuid)
        H.stopTask (UUID.toText $ untag uuid)

  (result, tasks, events) <- runTestApp (untag uuid, fakeTime) op

  let expectedResult = H.FailedToModify
  assertEqual "puts a task back into pending" expectedResult result

  assertEqual "tasks count" 1 (Map.size tasks)
  assertEqual "task status" (Just H.Pending) (H.status <$> Map.lookup uuid tasks)

  assertEqual "events count" 3 (Seq.length events)

  let intent = H.StopTask uuid
  let ev = V.Event { V.timestamp = fakeTime, V.payload = intent}
  assertEqual "events" (Just ev) (Seq.lookup 2 events)
