{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.Remove (testRemove) where

import qualified Data.UUID        as UUID
import qualified Data.UUID.V4     as UUID
import qualified HTask.Core       as H

import           Data.Time        (Day (ModifiedJulianDay), UTCTime (..))

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testRemove :: TestTree
testRemove = testGroup "remove"
  [ testCase "reports success when removing a task" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        H.removeTask (H.taskUuidToText taskId)

      let result = getResult output
      isSuccess result @? "puts a task back into pending"


  , testCase "removes the task" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess taskId) <- H.addTask "some task"
        H.removeTask (H.taskUuidToText taskId)

      let tasks = getTasks output
      mempty @=? tasks


  , testCase "fails if there is no matching event" $ do
      uuid <- UUID.nextRandom
      output <- runTestApp fakeTime $ H.removeTask (UUID.toText uuid)
      let result = getResult output
      H.FailedToFind @=? result

  ]


  where
    isSuccess (H.ModifySuccess _) = True
    isSuccess _                   = False
