{-# LANGUAGE OverloadedStrings #-}

module APITests.List
  ( testList
  ) where

import qualified Effects                   as F
import qualified HTask.API                 as API

import           Data.Time                 (Day (ModifiedJulianDay),
                                            UTCTime (..))
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
  uuid <- F.uuidGen
  x <- runApi (uuid, fakeTime) API.listTasks
  assertEqual "expecting success" [] x


storesCreatedTask :: TestTree
storesCreatedTask = testCase "stores the created task" $ do
  uuid <- F.uuidGen
  x <- runTasks (uuid, fakeTime) API.listTasks
  assertEqual "expecting one task" [] x


rollsBackOnWriteFailure :: TestTree
rollsBackOnWriteFailure = testCase "does not store task on write failure" $ do
  uuid <- F.uuidGen
  (_, x) <- runWriteFailure (uuid, fakeTime) API.listTasks
  assertEqual "expecting nothing" [] x


writesEvent :: TestTree
writesEvent = testCase "stores one event" $ do
  uuid <- F.uuidGen
  x <- runEventLog (uuid, fakeTime) API.listTasks
  assertEqual "expecting 'add-task' intent" [] x
