{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Core.List (testList) where

import qualified HTask.Core         as H

import           Data.Time          (Day (ModifiedJulianDay), UTCTime (..))

import           HTask.Core.TestApp
import           Test.Tasty
import           Test.Tasty.HUnit


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testList :: TestTree
testList = testGroup "list"
  [ testCase "returns empty when there are no tasks" $ do
      output <- runTestApp fakeTime H.listTasks
      let tasks = getResult output
      0 @=? length tasks


  , testCase "returns one task when created" $ do
      output <- runTestApp fakeTime $ do
        _ <- H.addTask "some task"
        H.listTasks

      let tasks = getResult output
      1 @=? length tasks


  , testCase "returns many tasks" $ do
      output <- runTestApp fakeTime $ do
        _ <- H.addTask "task 1"
        _ <- H.addTask "task 2"
        _ <- H.addTask "task 3"
        H.listTasks

      let tasks = getResult output
      3 @=? length tasks
  ]
