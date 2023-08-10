{-# LANGUAGE OverloadedStrings #-}

module Tests.Core.List (testList) where

import qualified Data.List        as List
import qualified HTask.Core       as H

import           Control.Monad    (void)
import           Data.Time        (Day (ModifiedJulianDay), UTCTime (..))

import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestApp


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testList :: TestTree
testList = testGroup "list"
  [ testCase "returns empty when there are no tasks" $ do
      output <- runTestApp fakeTime H.listTasks
      let result = getResult output
      mempty @=? result


  , testCase "returns one task when created" $ do
      output <- runTestApp fakeTime $ do
        void $ H.addTask "list test"
        H.listTasks

      let result = getResult output
      1 @=? List.length result


  , testCase "returns many tasks" $ do
      output <- runTestApp fakeTime $ do
        void $ H.addTask "one"
        void $ H.addTask "two"
        void $ H.addTask "three"
        H.listTasks

      let result = getResult output
      3 @=? List.length result

  ]
