{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.Stop (testStop) where

import qualified Data.Foldable      as Foldable
import qualified Data.Map.Strict    as Map
import qualified Data.Sequence      as Seq
import qualified Data.UUID          as UUID
import qualified Data.UUID.V4       as UUID
import qualified HTask.Core         as H
import qualified Leadpixel.Events   as V

import           Control.Monad      (void)
import           Data.Map.Strict    (Map)
import           Data.Sequence      (Seq)
import           Data.Tagged
import           Data.Time          (UTCTime)
import           Data.UUID          (UUID)
import           Leadpixel.Provider

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = read "2023-08-09 11:21:00 UTC"


testStop :: TestTree
testStop = testGroup "stop"
  [ cannotStopNonExistentEvent
  , canStopEvent
  , canStopEvent''
  ]


cannotStopNonExistentEvent :: TestTree
cannotStopNonExistentEvent = testCase "fails if there is no matching event" $ do
  uuid <- UUID.nextRandom
  let op = H.stopTask (UUID.toText uuid)

  (result, tasks, events) <- runFish (uuid, fakeTime) op

  assertEqual "expecting failure" H.FailedToFind result
  assertEqual "expecting no tasks" mempty tasks
  assertEqual "expecting no events" mempty events


canStopEvent :: TestTree
canStopEvent = testCase "reports success when stopping a task" $ do
  uuid <- Tagged <$> UUID.nextRandom
  let op = do
        void $ H.addTask "some task"
        H.stopTask (UUID.toText $ untag uuid)

  (result, tasks, events) <- runFish (untag uuid, fakeTime) op

  let expectedResult = H.ModifySuccess $ H.Task
        { H.taskUuid = uuid
        , H.description = "some task"
        , H.createdAt = fakeTime
        , H.status = H.Pending
        }

  assertEqual "puts a task back into pending" expectedResult result

  assertEqual "tasks count" 1 (Map.size tasks)
  assertEqual "task status" (Just H.Pending) (H.status <$> Map.lookup uuid tasks)

  assertEqual "events count" 2 (Seq.length events)
  assertEqual "events" (Just fakeTime) (V.timestamp <$> Seq.lookup 1 events)
  assertEqual "events" (Just uuid) (H.detailRef . V.payload <$> Seq.lookup 1 events)
  assertEqual "events" (Just $ H.StopTask uuid) (H.intent . V.payload <$> Seq.lookup 1 events)

  let detail = H.TaskEventDetail { H.detailRef = uuid, H.intent = H.StopTask uuid  }
  let ev = V.Event { V.timestamp = fakeTime, V.payload = detail}
  assertEqual "events" (Just ev) (Seq.lookup 1 events)


canStopEvent'' :: TestTree
canStopEvent'' = testCase "cannot stop a stopped task" $ do
  uuid <- UUID.nextRandom
  let op = do
        _ <- H.addTask "some task"
        _ <- H.stopTask (UUID.toText uuid)
        H.stopTask (UUID.toText uuid)

  (result, tasks, events) <- runFish (uuid, fakeTime) op

  let expected =
        [ H.AddTask "some task"
        , H.StopTask (Tagged uuid)
        , H.StopTask (Tagged uuid)
        ]

  assertEqual "expecting one 'add-task' intent"
    expected
    (H.intent . V.payload <$> Foldable.toList events)
