{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module APITests.Remove
  ( testRemove
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
import           Leadpixel.Provider
import           Test.Tasty
import           Test.Tasty.HUnit


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testRemove :: TestTree
testRemove = testGroup "remove"
  [ canRemoveEvent
  , canRemoveEvent'
  , canRemoveEvent''
  , cannotRemoveNonExistentEvent
  ]


op :: (MonadIO m, API.CanAddTask m, API.CanModifyTask m) => UUID -> m API.ModifyResult
op uuid = API.addTask "some task" >> API.removeTask (UUID.toText uuid)


canRemoveEvent :: TestTree
canRemoveEvent = testCase "reports success when removing a task" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) (op uuid)
  assertEqual "can remove" (f uuid) x

  where
    f uuid = API.ModifySuccess
      ( H.Task
        { H.taskRef = Tagged uuid
        , H.description = "some task"
        , H.createdAt = fakeTime
        , H.status = H.Pending
        }
      )


canRemoveEvent' :: TestTree
canRemoveEvent' = testCase "removes the task" $ do
  uuid <- UUID.nextRandom
  x <- runTasks (uuid, fakeTime) (op uuid)
  assertEqual "can remove" [] x


canRemoveEvent'' :: TestTree
canRemoveEvent'' = testCase "cannot remove a removeped task" $ do
  uuid <- UUID.nextRandom
  x <- runEventLog (uuid, fakeTime) (op uuid >> API.removeTask (UUID.toText uuid))
  assertEqual "expecting one 'add-task' intent"
    [ TV.AddTask "some task"
    , TV.RemoveTask (Tagged uuid)
    ]
    (TV.intent . V.payload <$> x)


-- canRemoveEvent''' :: TestTree
-- canRemoveEvent''' = testCase "cannot remove a removeped task" $ do
--   uuid <- UUID.nextRandom
--   x <- runTasks (uuid, fakeTime) (op uuid >> API.removeTask (Tagged uuid))
--   assertEqual "can remove"
--     [ H.Task
--       { H.taskRef = Tagged uuid
--       , H.description = "some task"
--       , H.createdAt = fakeTime
--       , H.status = H.InProgress
--       }
--     ]
--     x


cannotRemoveNonExistentEvent :: TestTree
cannotRemoveNonExistentEvent = testCase "fails if there is no matching event" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) (API.removeTask (UUID.toText uuid))
  assertEqual "expecting failure" API.FailedToFind x
