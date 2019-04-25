{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module HTaskTests.Log
  ( test_log
  ) where

import Data.Maybe
import Data.Tagged
import Event
import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import qualified Data.UUID as UUID
import qualified HTask as H


newtype LogTestMonad m a = LogTest
  { runLog :: R.ReaderT [H.Task] (W.WriterT [BS.ByteString] m) a
  } deriving (Functor, Applicative, Monad)

instance (Monad m, CanTime m) => CanTime (LogTestMonad m) where
  now = LogTest $ R.lift $ W.lift now

instance (Monad m, CanUuid m) => CanUuid (LogTestMonad m) where
  uuidGen = LogTest $ R.lift $ W.lift uuidGen

instance (Monad m) => HasEventSink (LogTestMonad m) where
  writeEvent ev = LogTest $ W.tell [ A.encode ev ]

instance (Monad m) => H.HasTasks (LogTestMonad m) where
  getTasks = LogTest R.ask
  addNewTask _t = LogTest $ pure True
  updateExistingTask r _f = LogTest $ R.asks (elem r . fmap H.taskRef)
  removeTaskRef r = LogTest $ R.asks (elem r . fmap H.taskRef)


extractLog :: (Monad m) => [H.Task] -> LogTestMonad m a -> m [H.TaskEvent]
extractLog xs op
  = catMaybes . fmap A.decode <$> W.execWriterT ( R.runReaderT (runLog op) xs )


randomTask :: H.Task
randomTask = H.Task
  { H.taskRef = Tagged UUID.nil
  , H.description = undefined
  , H.createdAt = undefined
  , H.status = H.Pending
  }


test_log :: TestTree
test_log = testGroup "logs"
  [ listingEmptyTasks
  , adding01Tasks
  , adding02Tasks
  , startingTask
  , startingNonTask
  , completingTask
  , removingTask
  ]


listingEmptyTasks :: TestTree
listingEmptyTasks = testCase "listing empty tasks" $ do
  ts <- extractLog [] $ pure ()
  assertEqual "expecting no logs" 0 (length ts)


adding01Tasks :: TestTree
adding01Tasks = testCase "adding one task" $ do
  ts <- extractLog [] $ H.addTask "some task"
  assertEqual "expecting one log entry" 1 (length ts)
  assertEqual "expecting 'add-task' intent" (H.AddTask "some task") (H.intent $ payload $ head ts)


adding02Tasks :: TestTree
adding02Tasks = testCase "adding two tasks" $ do
  ts <- extractLog [] $ H.addTask "some task" >> H.addTask "some other task"
  assertEqual "expecting two log entries" 2 (length ts)
  assertEqual "expecting 'add-task' intent" (H.AddTask "some task") (H.intent $ payload $ head ts)
  assertEqual "expecting 'add-task' intent" (H.AddTask "some other task") (H.intent $ payload $ ts !! 1)


startingTask :: TestTree
startingTask = testCase "starting a task" $ do
  let ref = Tagged UUID.nil
  ts <- extractLog [ randomTask ] $ H.startTask ref
  assertEqual "expecting one log entry" 1 (length ts)
  assertEqual "expecting 'start-task' intent" (H.StartTask ref) (H.intent $ payload $ head ts)


startingNonTask :: TestTree
startingNonTask = testCase "updating a non-event does not log the action" $ do
  ts <- extractLog [] $ H.startTask (Tagged UUID.nil)
  assertEqual "expecting no log entries" 0 (length ts)


completingTask :: TestTree
completingTask = testCase "completing a task" $ do
  let ref = Tagged UUID.nil
  ts <- extractLog [ randomTask ] $ H.completeTask ref
  assertEqual "expecting one log entry" 1 (length ts)
  assertEqual "expecting 'complete-task' intent" (H.CompleteTask ref) (H.intent $ payload $ head ts)


removingTask :: TestTree
removingTask = testCase "deleting a task" $ do
  let ref = Tagged UUID.nil
  ts <- extractLog [ randomTask ] $ H.removeTask ref
  assertEqual "expecting one log entry" 1 (length ts)
  assertEqual "expecting 'remove-task' intent" (H.RemoveTask ref) (H.intent $ payload $ head ts)
