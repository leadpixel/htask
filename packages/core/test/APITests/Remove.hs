{-# LANGUAGE OverloadedStrings #-}

module APITests.Remove
  ( testRemove
  ) where

import qualified Effects                   as F
import qualified HTask.Task                as H
import qualified Events                    as V
import qualified HTask.API                 as API
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


testRemove :: TestTree
testRemove = testGroup "remove"
  [ canRemoveEvent
  , canRemoveEvent'
  , canRemoveEvent''
  , cannotRemoveNonExistentEvent
  ]


op :: (API.CanAddTask m, API.CanModifyTask m) => UUID -> m API.ModifyResult
op uuid = API.addTask "some task" >> API.removeTask (UUID.toText uuid)


canRemoveEvent :: TestTree
canRemoveEvent = testCase "reports success when removing a task" $ do
  uuid <- F.uuidGen
  x <- runApi (uuid, fakeTime) (op uuid)
  assertEqual "" (f uuid) x
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
  uuid <- F.uuidGen
  x <- runTasks (uuid, fakeTime) (op uuid)
  assertEqual "" [] x


canRemoveEvent'' :: TestTree
canRemoveEvent'' = testCase "cannot remove a removeped task" $ do
  uuid <- F.uuidGen
  x <- runEventLog (uuid, fakeTime) (op uuid >> API.removeTask (UUID.toText uuid))
  assertEqual "expecting one 'add-task' intent"
    [ TV.AddTask "some task"
    , TV.RemoveTask (Tagged uuid)
    ]
    (TV.intent . V.payload <$> x)


-- canRemoveEvent''' :: TestTree
-- canRemoveEvent''' = testCase "cannot remove a removeped task" $ do
--   uuid <- F.uuidGen
--   x <- runTasks (uuid, fakeTime) (op uuid >> API.removeTask (Tagged uuid))
--   assertEqual ""
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
  uuid <- F.uuidGen
  x <- runApi (uuid, fakeTime) (API.removeTask (UUID.toText uuid))
  assertEqual "expecting failure" API.FailedToFind x
