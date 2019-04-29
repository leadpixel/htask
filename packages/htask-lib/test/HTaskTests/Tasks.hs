{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module HTaskTests.Tasks
  ( test_tasks
  ) where

import qualified Control.Monad.State as S
import           Data.Tagged
import qualified Data.UUID           as UUID
import           Event
import qualified HTask               as H
import           Test.Tasty
import           Test.Tasty.HUnit


newtype TaskTestMonad m a = TaskTest
  { runTask :: (S.StateT H.Tasks m) a
  } deriving (Functor, Applicative, Monad)

instance (Monad m, CanTime m) => CanTime (TaskTestMonad m) where
  now = TaskTest $ S.lift now

instance (Monad m, CanUuid m) => CanUuid (TaskTestMonad m) where
  uuidGen = TaskTest $ S.lift uuidGen

instance (Monad m) => H.HasTasks (TaskTestMonad m) where
  getTasks = TaskTest H.getTasks
  addNewTask t = TaskTest $ H.addNewTask t
  updateExistingTask t f = TaskTest $ H.updateExistingTask t f
  removeTaskRef t = TaskTest $ H.removeTaskRef t

instance (Monad m) => HasEventSink (TaskTestMonad m) where
  writeEvent _k = pure ()


extractTasks :: TaskTestMonad IO a -> IO H.Tasks
extractTasks op
  = S.evalStateT
      (runTask op >> H.listTasks)
      H.emptyTasks


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
    H.addTask "some task"
  assertEqual "expecting one task" 1 (length ts)


adding02Tasks :: TestTree
adding02Tasks = testCase "adding two tasks" $ do
  ts <- extractTasks $ do
    _ <- H.addTask "some task"
    H.addTask "some other task"
  assertEqual "expecting two tasks" 2 (length ts)


startingTask :: TestTree
startingTask = testCase "starting a task" $ do
  ts <- extractTasks $ do
    ref <- H.addTask "some task"
    case ref of
      Left e  -> pure (Left e)
      Right v -> H.startTask v
  assertEqual "expecting one task" 1 (length ts)
  assertEqual "expecting matching task" "some task" (H.description $ head ts)
  assertEqual "expecting started task" H.InProgress (H.status $ head ts)


startingNonTask :: TestTree
startingNonTask = testCase "starting non-existent task does not error" $ do
  ts <- extractTasks $
    H.startTask (Tagged UUID.nil)
  assertEqual "expecting no tasks" 0 (length ts)


completingTask :: TestTree
completingTask = testCase "completing a task" $ do
  ts <- extractTasks $ do
    ref <- H.addTask "some task"
    case ref of
      Left e  -> pure (Left e)
      Right v -> H.completeTask v
  assertEqual "expecting one task" 1 (length ts)
  assertEqual "expecting matching task" "some task" (H.description $ head ts)
  assertEqual "expecting started task" H.Complete (H.status $ head ts)


deletingTask :: TestTree
deletingTask = testCase "deleting a task" $ do
  ts <- extractTasks $ do
    ref <- H.addTask "some task"
    case ref of
      Left e  -> pure (Left e)
      Right v -> H.removeTask v
  assertEqual "expecting no tasks" 0 (length ts)
