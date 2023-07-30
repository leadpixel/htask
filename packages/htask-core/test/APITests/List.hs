{-# LANGUAGE OverloadedStrings #-}

module APITests.List
  ( testList
  ) where

import qualified Data.UUID.V4              as UUID
import qualified HTask.Core                as H

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
  [ testGroup "success"
      [ returnsCreatedUuid
      , storesCreatedTask
      , rollsBackOnWriteFailure
      , writesEvent
      ]
  ]


returnsCreatedUuid :: TestTree
returnsCreatedUuid = testCase "returns the created uuid on success" $ do
  uuid <- UUID.nextRandom
  x <- runApi (uuid, fakeTime) H.listTasks
  assertEqual "expecting success" mempty x


storesCreatedTask :: TestTree
storesCreatedTask = testCase "stores the created task" $ do
  uuid <- UUID.nextRandom
  x <- runTasks (uuid, fakeTime) H.listTasks
  assertEqual "expecting one task" mempty x


rollsBackOnWriteFailure :: TestTree
rollsBackOnWriteFailure = testCase "does not store task on write failure" $ do
  uuid <- UUID.nextRandom
  (_, x) <- runWriteFailure (uuid, fakeTime) H.listTasks
  assertEqual "expecting nothing" mempty x


writesEvent :: TestTree
writesEvent = testCase "stores one event" $ do
  uuid <- UUID.nextRandom
  x <- runEventLog (uuid, fakeTime) H.listTasks
  assertEqual "expecting 'add-task' intent" mempty x
