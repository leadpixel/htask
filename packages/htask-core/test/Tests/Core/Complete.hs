{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.Complete (testComplete) where

import qualified Data.UUID        as UUID
import qualified Data.UUID.V4     as UUID
import qualified HTask.Core       as H

import           Data.Tagged      (Tagged (..))
import           Data.Time        (Day (ModifiedJulianDay), UTCTime (..))

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testComplete :: TestTree
testComplete = testGroup "complete"
  [ returnsCreatedUuid
  , failsWhenUnableToFindMatch
  ]


returnsCreatedUuid :: TestTree
returnsCreatedUuid = testCase "returns the created uuid on success" $ do
  uuid <- UUID.nextRandom
  x <- getResult <$> runTestApp (uuid, fakeTime) (H.addTask "some task" >> H.completeTask (UUID.toText uuid))
  f uuid @=? x
    where
      f uuid = H.ModifySuccess
        ( H.Task
          { H.taskUuid = Tagged uuid
          , H.description = "some task"
          , H.createdAt = fakeTime
          , H.status = H.Complete
          }
        )


failsWhenUnableToFindMatch :: TestTree
failsWhenUnableToFindMatch = testCase "fails when unable to find a matching task" $ do
  uuid <- UUID.nextRandom
  x <- getResult <$> runTestApp (uuid, fakeTime) (H.addTask "some task" >> H.completeTask "unknown")
  assertEqual "expecting failed to find" H.FailedToFind x
