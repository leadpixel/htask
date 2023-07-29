{-# LANGUAGE OverloadedStrings #-}

module APITests.List
  ( testList
  ) where

import qualified Data.UUID.V4              as UUID
import qualified HTask.Core.API            as API

import           Data.Time                 (Day (ModifiedJulianDay),
                                            UTCTime (..))
import           Leadpixel.Provider
import           Test.QuickCheck.Instances ()

import           APITestMonad
import           Test.Tasty
import           Test.Tasty.HUnit


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testList :: TestTree
testList = testGroup "add"
  [ testSuccess
  , testFailure
  ]

  where
    testSuccess = testGroup "success"
      [ returnsCreatedUuid
      , storesCreatedTask
      , rollsBackOnWriteFailure
      , writesEvent
      ]

    testFailure = testGroup "failure"
      []


returnsCreatedUuid :: TestTree
returnsCreatedUuid = testCase "returns the created uuid on success" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) API.listTasks
  assertEqual "expecting success" [] x


storesCreatedTask :: TestTree
storesCreatedTask = testCase "stores the created task" $ do
  uuid <- UUID.nextRandom
  x <- runTasks (uuid, fakeTime) API.listTasks
  assertEqual "expecting one task" [] x


rollsBackOnWriteFailure :: TestTree
rollsBackOnWriteFailure = testCase "does not store task on write failure" $ do
  uuid <- UUID.nextRandom
  (_, x) <- runWriteFailure (uuid, fakeTime) API.listTasks
  assertEqual "expecting nothing" [] x


writesEvent :: TestTree
writesEvent = testCase "stores one event" $ do
  uuid <- UUID.nextRandom
  x <- runEventLog (uuid, fakeTime) API.listTasks
  assertEqual "expecting 'add-task' intent" [] x
