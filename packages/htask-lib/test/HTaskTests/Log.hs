{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HTaskTests.Log
  ( test_log
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Monad.State as S
import Data.Tagged
import qualified Control.Monad.Writer as W
import qualified HTask as H
import qualified Data.UUID as UUID


type LogTestMonad = W.WriterT [H.TaskEvent] (S.StateT H.Tasks IO)

instance H.CanTime LogTestMonad where
  now = W.lift (S.lift H.now)

instance H.CanUuid LogTestMonad where
  uuidGen = W.lift (S.lift H.uuidGen)


extractLog :: LogTestMonad a -> IO [H.TaskEvent]
extractLog op
  = S.evalStateT
      (W.execWriterT op)
      H.emptyTasks


test_log :: TestTree
test_log = testGroup "logs"
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
  ts <- extractLog $ pure ()
  assertEqual "expecting no logs" 0 (length ts)


adding01Tasks :: TestTree
adding01Tasks = testCase "adding one task" $ do
  ts <- extractLog $ H.addTask "some task"
  assertEqual "expecting one log entry" 1 (length ts)


adding02Tasks :: TestTree
adding02Tasks = testCase "adding two tasks" $ do
  ts <- extractLog $ do
    _ <- H.addTask "some task"
    H.addTask "some other task"
  assertEqual "expecting two log entries" 2 (length ts)


startingTask :: TestTree
startingTask = testCase "starting a task" $ do
  ts <- extractLog $ do
    ref <- H.addTask "some task"
    case ref of
      Left e -> pure (Left e)
      Right v -> H.startTask v
  assertEqual "expecting two log entries" 2 (length ts)
  -- assertEqual "expecting matching task" "some task" (show $ head ts)
  -- assertEqual "expecting started task" H.InProgress (H.status $ head ts)


startingNonTask :: TestTree
startingNonTask = testCase "starting non-existent task does not error" $ do
  ts <- extractLog $
    H.startTask (Tagged UUID.nil)
  assertEqual "expecting no log entries" 0 (length ts)


completingTask :: TestTree
completingTask = testCase "completing a task" $ do
  ts <- extractLog $ do
    ref <- H.addTask "some task"
    case ref of
      Left e -> pure (Left e)
      Right v -> H.completeTask v
  assertEqual "expecting two log entries" 2 (length ts)
  -- assertEqual "expecting matching task" "some task" (H.description $ head ts)
  -- assertEqual "expecting started task" H.Complete (H.status $ head ts)


deletingTask :: TestTree
deletingTask = testCase "deleting a task" $ do
  ts <- extractLog $ do
    ref <- H.addTask "some task"
    case ref of
      Left e -> pure (Left e)
      Right v -> H.deleteTask v
  assertEqual "expecting two log entries" 2 (length ts)
