{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module APITests.Stop
  ( testStop
  ) where

import qualified Data.UUID                 as UUID
import qualified Data.UUID.V4              as UUID
import qualified Events                    as V
import qualified HTask.Core.API            as API
import qualified HTask.Core.Task           as H
import qualified HTask.Core.TaskEvent      as TV

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Tagged               (Tagged (..))
import           Data.Time                 (Day (ModifiedJulianDay),
                                            UTCTime (..))
import           Data.UUID                 (UUID)
import           Test.QuickCheck.Instances ()

import           APITestMonad
import           Test.Tasty
import           Test.Tasty.HUnit


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testStop :: TestTree
testStop = testGroup "stop"
  [ canStopEvent
  , canStopEvent'
  , canStopEvent''
  , cannotStopNonExistentEvent
  ]


op :: (MonadIO m, API.CanAddTask m, API.CanModifyTask m) => UUID -> m API.ModifyResult
op uuid = API.addTask "some task" >> API.stopTask (UUID.toText uuid)


canStopEvent :: TestTree
canStopEvent = testCase "reports success when stopping a task" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) (op uuid)
  assertEqual "can stop" (f uuid) x
    where
      f uuid = API.ModifySuccess
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
    [ H.Task
      { H.taskUuid = Tagged uuid
      , H.description = "some task"
      , H.createdAt = fakeTime
      , H.status = H.Pending
      }
    ]
    x


canStopEvent'' :: TestTree
canStopEvent'' = testCase "cannot stop a stopped task" $ do
  uuid <- UUID.nextRandom
  x <- runEventLog (uuid, fakeTime) (op uuid >> API.stopTask (UUID.toText uuid))
  assertEqual "expecting one 'add-task' intent"
    [ TV.AddTask "some task"
    , TV.StopTask (Tagged uuid)
    , TV.StopTask (Tagged uuid)
    ]
    (TV.intent . V.payload <$> x)


-- canStopEvent''' :: TestTree
-- canStopEvent''' = testCase "cannot stop a stopped task" $ do
--   uuid <- UUID.nextRandom
--   x <- runTasks (uuid, fakeTime) (op uuid >> API.stopTask (UUID.toText uuid))
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
  x <- runApi (uuid, fakeTime) (API.stopTask (UUID.toText uuid))
  assertEqual "expecting failure" API.FailedToFind x
