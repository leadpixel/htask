{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.Stop (testStop) where

import qualified Data.UUID                 as UUID
import qualified Data.UUID.V4              as UUID
import qualified HTask.Core                as H
import qualified Leadpixel.Events          as V

import           Data.Tagged               (Tagged (..))
import           Data.Time                 (Day (ModifiedJulianDay),
                                            UTCTime (..))
import           Data.UUID                 (UUID)
import           Leadpixel.Provider
import           Test.QuickCheck.Instances ()

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testStop :: TestTree
testStop = testGroup "stop"
  [ canStopEvent
  , canStopEvent'
  , canStopEvent''
  , cannotStopNonExistentEvent
  ]


op :: (Provider UTCTime m, Provider UUID m, H.HasTasks m, V.HasEventSink m) => UUID -> m H.ModifyResult
op uuid = H.addTask "some task" >> H.stopTask (UUID.toText uuid)


canStopEvent :: TestTree
canStopEvent = testCase "reports success when stopping a task" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) (op uuid)
  assertEqual "can stop" (f uuid) x
    where
      f uuid = H.ModifySuccess
        ( H.Task
          { H.taskUuid = Tagged uuid
          , H.description = "some task"
          , H.createdAt = fakeTime
          , H.status = H.Pending
          }
        )


canStopEvent' :: TestTree
canStopEvent' = testCase "marks the task as pending" $ do
  uuid <- UUID.nextRandom
  x <- runTasks (uuid, fakeTime) (op uuid)
  assertEqual "can stop"
    (pure  H.Task
      { H.taskUuid = Tagged uuid
      , H.description = "some task"
      , H.createdAt = fakeTime
      , H.status = H.Pending
      }
    )
    x


canStopEvent'' :: TestTree
canStopEvent'' = testCase "cannot stop a stopped task" $ do
  uuid <- UUID.nextRandom
  x <- runEventLog (uuid, fakeTime) (op uuid >> H.stopTask (UUID.toText uuid))
  assertEqual "expecting one 'add-task' intent"
    [ H.AddTask "some task"
    , H.StopTask (Tagged uuid)
    , H.StopTask (Tagged uuid)
    ]
    (H.intent . V.payload <$> x)


-- canStopEvent''' :: TestTree
-- canStopEvent''' = testCase "cannot stop a stopped task" $ do
--   uuid <- UUID.nextRandom
--   x <- runTasks (uuid, fakeTime) (op uuid >> H.stopTask (UUID.toText uuid))
--   assertEqual "can stop"
--     [ H.Task
--       { H.taskUuid = Tagged uuid
--       , H.description = "some task"
--       , H.createdAt = fakeTime
--       , H.status = H.Pending
--       }
--     ]
--     x


cannotStopNonExistentEvent :: TestTree
cannotStopNonExistentEvent = testCase "fails if there is no matching event" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) (H.stopTask (UUID.toText uuid))
  assertEqual "expecting failure" H.FailedToFind x
