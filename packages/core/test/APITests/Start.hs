{-# LANGUAGE OverloadedStrings #-}

module APITests.Start
  ( testStart
  ) where

import qualified Effects                   as F
import qualified Events                    as V
import qualified HTask.API                 as API
import qualified HTask.Task                as H
import qualified HTask.TaskEvent           as TV
import qualified Data.UUID                 as UUID

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


op :: (API.CanAddTask m, API.CanModifyTask m) => UUID -> m API.ModifyResult
op uuid = API.addTask "some task" >> API.startTask (UUID.toText uuid)


canStartEvent :: TestTree
canStartEvent = testCase "reports success when starting a task" $ do
  uuid <- F.uuidGen
  x <- runApi (uuid, fakeTime) (op uuid)
  assertEqual "" (f uuid) x
    where
      f uuid = API.ModifySuccess
        ( H.Task
          { H.taskRef = Tagged uuid
          , H.description = "some task"
          , H.createdAt = fakeTime
          , H.status = H.InProgress
          }
        )


canStartEvent' :: TestTree
canStartEvent' = testCase "marks the task as in-progress" $ do
  uuid <- F.uuidGen
  x <- runTasks (uuid, fakeTime) (op uuid)
  assertEqual ""
    [ H.Task
      { H.taskRef = Tagged uuid
      , H.description = "some task"
      , H.createdAt = fakeTime
      , H.status = H.InProgress
      }
    ]
    x


canStartEvent'' :: TestTree
canStartEvent'' = testCase "cannot start a started task" $ do
  uuid <- F.uuidGen
  x <- runEventLog (uuid, fakeTime) (op uuid >> API.startTask (UUID.toText uuid))
  assertEqual "expecting one 'add-task' intent"
    [ TV.AddTask "some task"
    , TV.StartTask (Tagged uuid)
    , TV.StartTask (Tagged uuid)
    ]
    (TV.intent . V.payload <$> x)


-- canStartEvent''' :: TestTree
-- canStartEvent''' = testCase "cannot start a startped task" $ do
--   uuid <- F.uuidGen
--   x <- runTasks (uuid, fakeTime) (op uuid >> API.startTask (UUID.toText uuid))
--   assertEqual ""
--     [ H.Task
--       { H.taskRef = Tagged uuid
--       , H.description = "some task"
--       , H.createdAt = fakeTime
--       , H.status = H.InProgress
--       }
--     ]
--     x


cannotStartNonExistentEvent :: TestTree
cannotStartNonExistentEvent = testCase "fails if there is no matching event" $ do
  uuid <- F.uuidGen
  x <- runApi (uuid, fakeTime) (API.startTask (UUID.toText uuid))
  assertEqual "expecting failure" API.FailedToFind x
