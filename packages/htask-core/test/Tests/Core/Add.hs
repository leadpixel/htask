{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Core.Add (testAdd) where

import qualified Data.Map         as Map
import qualified Data.Time        as Time
import qualified HTask.Core       as H

import           Control.Monad    (void)
import           Data.Time        (UTCTime (..))

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = UTCTime (Time.ModifiedJulianDay 0) 0


testAdd :: TestTree
testAdd = testGroup "add"
  [ testCase "returns the created uuid on success" $ do
      output <- runTestApp fakeTime $ H.addTask "some task"
      let result = getResult output
      isSuccess result @? ""


  , testCase "tasks start out pending" $ do
      output <- runTestApp fakeTime $ H.addTask "some task"
      let tasks = getTasks output
      let (H.AddSuccess taskId) = getResult output

      1 @=? Map.size tasks
      Just H.Pending @=? (H.status <$> Map.lookup taskId tasks)


  , testCase "creates a unique id for each task" $ do
      output <- runTestApp fakeTime $ do
        (H.AddSuccess a) <- H.addTask "task one"
        (H.AddSuccess b) <- H.addTask "task two"
        pure (a, b)

      let (a, b) = getResult output
      (a /= b) @? ""


  , testCase "ignores duplicates" $ do
      output <- runTestApp fakeTime $ do
        void $ H.addTask "some task"
        void $ H.addTask "some task"
        H.listTasks

      1 @=? length (getResult output)

  ]


  where
    isSuccess (H.AddSuccess _) = True
    isSuccess _                = False
