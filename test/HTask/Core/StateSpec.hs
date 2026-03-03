{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Core.StateSpec (tests) where

import qualified HTask.Core         as Core

import           Data.Time          (Day (ModifiedJulianDay), UTCTime (..))

import           HTask.Core.TestApp
import           Test.Tasty
import           Test.Tasty.HUnit

fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0

tests :: TestTree
tests = testGroup "StateMachine"
  [ testCase "ID stability is preserved across state transitions" $ do
      output <- runTestApp fakeTime $ do
        _ <- Core.addTask "task 1"
        _ <- Core.addTask "task 2"
        _ <- Core.addTask "task 3"

        -- start task 2 and 3
        _ <- Core.startTask "2"
        _ <- Core.startTask "3"

        -- complete task 3
        _ <- Core.completeTask "3"

        -- ID 2 should still point to task 2 even though status changed
        Core.findTask "2"

      let result = getResult output
      Just "task 2" @=? (Core.description <$> result)
      Just Core.InProgress @=? (Core.status <$> result)

  , testCase "Multiple state transitions sequence" $ do
      output <- runTestApp fakeTime $ do
        (Core.AddSuccess taskId) <- Core.addTask "multi task"
        let tId = Core.taskUuidToText taskId

        -- Pending -> InProgress
        st1 <- Core.startTask tId
        -- InProgress -> Pending
        st2 <- Core.stopTask tId
        -- Pending -> InProgress
        st3 <- Core.startTask tId
        -- InProgress -> Complete
        st4 <- Core.completeTask tId

        pure (st1, st2, st3, st4)

      let (st1, st2, st3, st4) = getResult output

      isSuccess st1 @? "Expected first start to succeed"
      isSuccess st2 @? "Expected stop to succeed"
      isSuccess st3 @? "Expected second start to succeed"
      isSuccess st4 @? "Expected complete to succeed"

      case st4 of
        Core.ModifySuccess t -> Core.Complete @=? Core.status t
        _                    -> assertFailure "st4 should be ModifySuccess"

  ]
  where
    isSuccess (Core.ModifySuccess _) = True
    isSuccess _                      = False
