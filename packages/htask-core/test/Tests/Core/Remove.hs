{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.Remove (testRemove) where

import qualified Data.UUID                 as UUID
import qualified Data.UUID.V4              as UUID
import qualified HTask.Core                as H
import qualified Data.Sequence as Seq
import qualified Leadpixel.Events          as V

import           Data.Tagged               (Tagged (..))
import           Data.Time                 (Day (ModifiedJulianDay),
                                            UTCTime (..))
import           Data.UUID                 (UUID)
import           Leadpixel.Provider

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testRemove :: TestTree
testRemove = testGroup "remove"
  [ canRemoveEvent
  , canRemoveEvent'
  , canRemoveEvent''
  , cannotRemoveNonExistentEvent
  ]


op :: (Provider UTCTime m, Provider UUID m, H.HasTasks m, V.HasEventSink m) => UUID -> m H.ModifyResult
op uuid = H.addTask "some task" >> H.removeTask (UUID.toText uuid)


canRemoveEvent :: TestTree
canRemoveEvent = testCase "reports success when removing a task" $ do
  uuid <- UUID.nextRandom
  x <- getResult <$> runTestApp (uuid, fakeTime) (op uuid)
  assertEqual "can remove" (f uuid) x

  where
    f uuid = H.ModifySuccess
      ( H.Task
        { H.taskUuid = Tagged uuid
        , H.description = "some task"
        , H.createdAt = fakeTime
        , H.status = H.Pending
        }
      )


canRemoveEvent' :: TestTree
canRemoveEvent' = testCase "removes the task" $ do
  uuid <- UUID.nextRandom
  x <- getTasks <$> runTestApp (uuid, fakeTime) (op uuid)
  assertEqual "can remove" mempty x


canRemoveEvent'' :: TestTree
canRemoveEvent'' = testCase "cannot remove a removeped task" $ do
  uuid <- UUID.nextRandom
  x <- getEvents <$> runTestApp (uuid, fakeTime) (op uuid >> H.removeTask (UUID.toText uuid))

  let expectedEvents = Seq.fromList
        [ H.AddTask (Tagged uuid) "some task"
        , H.RemoveTask (Tagged uuid)
        ]
  assertEqual "expecting one 'add-task' intent" expectedEvents (V.payload <$> x)


-- canRemoveEvent''' :: TestTree
-- canRemoveEvent''' = testCase "cannot remove a removeped task" $ do
--   uuid <- UUID.nextRandom
--   x <- getTasks <$> runTestApp (uuid, fakeTime) (op uuid >> H.removeTask (Tagged uuid))
--   assertEqual "can remove"
--     [ H.Task
--       { H.taskUuid = Tagged uuid
--       , H.description = "some task"
--       , H.createdAt = fakeTime
--       , H.status = H.InProgress
--       }
--     ]
--     x


cannotRemoveNonExistentEvent :: TestTree
cannotRemoveNonExistentEvent = testCase "fails if there is no matching event" $ do
  uuid <- UUID.nextRandom
  x <- getResult <$> runTestApp (uuid, fakeTime) (H.removeTask (UUID.toText uuid))
  assertEqual "expecting failure" H.FailedToFind x
