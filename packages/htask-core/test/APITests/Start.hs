{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module APITests.Start
  ( testStart
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


testStart :: TestTree
testStart = testGroup "start"
  [ canStartEvent
  , canStartEvent'
  , canStartEvent''
  , cannotStartNonExistentEvent
  ]


op :: (MonadIO m, API.CanAddTask m, API.CanModifyTask m) => UUID -> m API.ModifyResult
op uuid = API.addTask "some task" >> API.startTask (UUID.toText uuid)


canStartEvent :: TestTree
canStartEvent = testCase "reports success when starting a task" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) (op uuid)
  assertEqual "can start" (f uuid) x

  where
    f uuid = API.ModifySuccess
      ( H.Task
        { H.taskUuid = Tagged uuid
        , H.description = "some task"
        , H.createdAt = fakeTime
        , H.status = H.InProgress
        }
      )


canStartEvent' :: TestTree
canStartEvent' = testCase "marks the task as in-progress" $ do
  uuid <- UUID.nextRandom
  x <- runTasks (uuid, fakeTime) (op uuid)
  assertEqual "can start"
    [ H.Task
      { H.taskUuid = Tagged uuid
      , H.description = "some task"
      , H.createdAt = fakeTime
      , H.status = H.InProgress
      }
    ]
    x


canStartEvent'' :: TestTree
canStartEvent'' = testCase "cannot start a started task" $ do
  uuid <- UUID.nextRandom
  x <- runEventLog (uuid, fakeTime) (op uuid >> API.startTask (UUID.toText uuid))
  assertEqual "expecting one 'add-task' intent"
    [ TV.AddTask "some task"
    , TV.StartTask (Tagged uuid)
    , TV.StartTask (Tagged uuid)
    ]
    (TV.intent . V.payload <$> x)


-- canStartEvent''' :: TestTree
-- canStartEvent''' = testCase "cannot start a startped task" $ do
--   uuid <- UUID.nextRandom
--   x <- runTasks (uuid, fakeTime) (op uuid >> API.startTask (UUID.toText uuid))
--   assertEqual "can start"
--     [ H.Task
--       { H.taskUuid = Tagged uuid
--       , H.description = "some task"
--       , H.createdAt = fakeTime
--       , H.status = H.InProgress
--       }
--     ]
--     x


cannotStartNonExistentEvent :: TestTree
cannotStartNonExistentEvent = testCase "fails if there is no matching event" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) (API.startTask (UUID.toText uuid))
  assertEqual "expecting failure" API.FailedToFind x
