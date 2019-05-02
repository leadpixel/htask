{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module HTaskTests.Tasks
  ( test_tasks
  ) where

import qualified Control.Monad.State as S
import qualified Data.UUID           as UUID
import qualified Event as V
import qualified HTask.Task as H
import qualified HTask.TaskContainer as HC
import qualified HTask.API as API

import           Data.Tagged (Tagged(..))

import           Test.Tasty
import           Test.Tasty.HUnit


newtype TaskTestMonad m a = TaskTest
  { runTask :: (S.StateT HC.Tasks m) a
  } deriving (Functor, Applicative, Monad)

instance (Monad m, V.CanTime m) => V.CanTime (TaskTestMonad m) where
  now = TaskTest $ S.lift V.now

instance (Monad m, V.CanUuid m) => V.CanUuid (TaskTestMonad m) where
  uuidGen = TaskTest $ S.lift V.uuidGen

instance (Monad m) => HC.HasTasks (TaskTestMonad m) where
  getTasks = TaskTest HC.getTasks
  addNewTask t = TaskTest $ HC.addNewTask t
  updateExistingTask t f = TaskTest $ HC.updateExistingTask t f
  removeTaskRef t = TaskTest $ HC.removeTaskRef t

instance (Monad m) => V.HasEventSink (TaskTestMonad m) where
  writeEvent _k = pure ()


extractTasks :: TaskTestMonad IO a -> IO HC.Tasks
extractTasks op
  = S.evalStateT
      (runTask op >> API.listTasks)
      HC.emptyTasks


test_tasks :: TestTree
test_tasks = testGroup "tasks"
  [ listingEmptyTasks
  , adding01Tasks
  , adding02Tasks
  , startingTask
  , startingNonTask
  , completingTask
  , deletingTask
  ]


listingEmptyTasks :: TestTree
listingEmptyTasks = testCase "listing empty tasks" $ do
  ts <- extractTasks (pure ())
  assertEqual "expecting no tasks" 0 (length ts)


adding01Tasks :: TestTree
adding01Tasks = testCase "adding one task" $ do
  ts <- extractTasks $
    API.addTask "some task"
  assertEqual "expecting one task" 1 (length ts)


adding02Tasks :: TestTree
adding02Tasks = testCase "adding two tasks" $ do
  ts <- extractTasks $ do
    _ <- API.addTask "some task"
    API.addTask "some other task"
  assertEqual "expecting two tasks" 2 (length ts)


startingTask :: TestTree
startingTask = testCase "starting a task" $ do
  ts <- extractTasks $ do
    ref <- API.addTask "some task"
    case ref of
      Left e  -> pure (Left e)
      Right v -> API.startTask v
  assertEqual "expecting one task" 1 (length ts)
  assertEqual "expecting matching task" "some task" (H.description $ head ts)
  assertEqual "expecting started task" H.InProgress (H.status $ head ts)


startingNonTask :: TestTree
startingNonTask = testCase "starting non-existent task does not error" $ do
  ts <- extractTasks $
    API.startTask (Tagged UUID.nil)
  assertEqual "expecting no tasks" 0 (length ts)


completingTask :: TestTree
completingTask = testCase "completing a task" $ do
  ts <- extractTasks $ do
    ref <- API.addTask "some task"
    case ref of
      Left e  -> pure (Left e)
      Right v -> API.completeTask v
  assertEqual "expecting one task" 1 (length ts)
  assertEqual "expecting matching task" "some task" (H.description $ head ts)
  assertEqual "expecting started task" H.Complete (H.status $ head ts)


deletingTask :: TestTree
deletingTask = testCase "deleting a task" $ do
  ts <- extractTasks $ do
    ref <- API.addTask "some task"
    case ref of
      Left e  -> pure (Left e)
      Right v -> API.removeTask v
  assertEqual "expecting no tasks" 0 (length ts)
