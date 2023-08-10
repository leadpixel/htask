{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.List (testList) where

import qualified Data.UUID.V4              as UUID
import qualified HTask.Core                as H

import           Data.Time                 (Day (ModifiedJulianDay),
                                            UTCTime (..))
import           Test.QuickCheck.Instances ()

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testList :: TestTree
testList = testGroup "list"
  [ returnsCreatedUuid
  , storesCreatedTask
  , rollsBackOnWriteFailure
  , writesEvent
  ]


returnsCreatedUuid :: TestTree
returnsCreatedUuid = testCase "returns the created uuid on success" $ do
  uuid <- UUID.nextRandom
  x <- getResult <$> runTestApp (uuid, fakeTime) H.listTasks
  assertEqual "expecting success" mempty x


storesCreatedTask :: TestTree
storesCreatedTask = testCase "stores the created task" $ do
  uuid <- UUID.nextRandom
  x <- getTasks <$> runTestApp (uuid, fakeTime) H.listTasks
  assertEqual "expecting one task" mempty x


rollsBackOnWriteFailure :: TestTree
rollsBackOnWriteFailure = testCase "does not store task on write failure" $ do
  uuid <- UUID.nextRandom
  (_, x) <- runWriteFailure (uuid, fakeTime) H.listTasks
  assertEqual "expecting nothing" mempty x


writesEvent :: TestTree
writesEvent = testCase "stores one event" $ do
  uuid <- UUID.nextRandom
  x <- getEvents <$> runTestApp (uuid, fakeTime) H.listTasks
  assertEqual "expecting 'add-task' intent" mempty x