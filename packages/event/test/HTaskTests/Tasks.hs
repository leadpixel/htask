{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HTaskTests.Tasks
  ( test_tasks
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W
import qualified HTask as H
import qualified Data.UUID as UUID
import Data.Tagged


type TaskTestMonad = W.WriterT [H.TaskEvent] (S.StateT H.Tasks IO)

instance H.CanTime TaskTestMonad where
  now = W.lift (S.lift H.now)

instance H.CanUuid TaskTestMonad where
  uuidGen = W.lift (S.lift H.uuidGen)


extractTasks :: TaskTestMonad a -> IO H.Tasks
extractTasks op
  = S.evalStateT
      (fst <$> W.runWriterT (op >> H.listTasks))
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
      Left e -> pure (Left e)
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
      Left e -> pure (Left e)
      Right v -> H.completeTask v
  assertEqual "expecting one task" 1 (length ts)
  assertEqual "expecting matching task" "some task" (H.description $ head ts)
  assertEqual "expecting started task" H.Complete (H.status $ head ts)


deletingTask :: TestTree
deletingTask = testCase "deleting a task" $ do
  ts <- extractTasks $ do
    ref <- H.addTask "some task"
    case ref of
      Left e -> pure (Left e)
      Right v -> H.removeTask v
  assertEqual "expecting no tasks" 0 (length ts)
