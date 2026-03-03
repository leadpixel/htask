{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Core.List (testList) where

import qualified HTask.Core         as Core

import           Data.Time          (Day (ModifiedJulianDay), UTCTime (..))

import           HTask.Core.TestApp
import           Test.Tasty
import           Test.Tasty.HUnit

fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0

testList :: TestTree
testList = testGroup "list"
  [ testCase "returns empty when there are no tasks" $ do
      output <- runTestApp fakeTime Core.listTasks
      let tasks = getResult output
      0 @=? length tasks

  , testCase "returns one task when created" $ do
      output <- runTestApp fakeTime $ do
        _ <- Core.addTask "some task"
        Core.listTasks

      let tasks = getResult output
      1 @=? length tasks

  , testCase "returns many tasks" $ do
      output <- runTestApp fakeTime $ do
        _ <- Core.addTask "task 1"
        _ <- Core.addTask "task 2"
        _ <- Core.addTask "task 3"
        Core.listTasks

      let tasks = getResult output
      3 @=? length tasks

  , testCase "finds a task by numeric id" $ do
      output <- runTestApp fakeTime $ do
        _ <- Core.addTask "task 1"
        _ <- Core.addTask "task 2"
        _ <- Core.addTask "task 3"
        Core.findTask "2"

      let result = getResult output
      Just "task 2" @=? (Core.description <$> result)
  ]
