{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module APITests.Start
  ( testStart
  ) where

import qualified Data.UUID                 as UUID
import qualified Data.UUID.V4              as UUID
import qualified Leadpixel.Events                    as V
import qualified HTask.Core                as H

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


op :: (MonadIO m, H.CanAddTask m, H.CanModifyTask m) => UUID -> m H.ModifyResult
op uuid = H.addTask "some task" >> H.startTask (UUID.toText uuid)


canStartEvent :: TestTree
canStartEvent = testCase "reports success when starting a task" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) (op uuid)
  assertEqual "can start" (f uuid) x

  where
    f uuid = H.ModifySuccess
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
    (pure H.Task
      { H.taskUuid = Tagged uuid
      , H.description = "some task"
      , H.createdAt = fakeTime
      , H.status = H.InProgress
      }
    )
    x


canStartEvent'' :: TestTree
canStartEvent'' = testCase "cannot start a started task" $ do
  uuid <- UUID.nextRandom
  x <- runEventLog (uuid, fakeTime) (op uuid >> H.startTask (UUID.toText uuid))
  assertEqual "expecting one 'add-task' intent"
    [ H.AddTask "some task"
    , H.StartTask (Tagged uuid)
    , H.StartTask (Tagged uuid)
    ]
    (H.intent . V.payload <$> x)


-- canStartEvent''' :: TestTree
-- canStartEvent''' = testCase "cannot start a startped task" $ do
--   uuid <- UUID.nextRandom
--   x <- runTasks (uuid, fakeTime) (op uuid >> H.startTask (UUID.toText uuid))
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
  x <- runApi (uuid, fakeTime) (H.startTask (UUID.toText uuid))
  assertEqual "expecting failure" H.FailedToFind x
