{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HTaskTests.Tasks
  where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W
import qualified HTask as H


type TaskTestMonad = W.WriterT H.EventLog (S.StateT H.Tasks IO)

instance H.CanTime TaskTestMonad where
  now = W.lift (S.lift H.now)

instance H.CanUuid TaskTestMonad where
  uuidGen = W.lift (S.lift H.uuidGen)


extractTasks :: TaskTestMonad H.Tasks -> IO H.Tasks
extractTasks op = do
  S.evalStateT
    (fst <$> W.runWriterT op)
    H.emptyTasks


test_tasks :: TestTree
test_tasks = testGroup "tasks"
  [ listingEmptyTasks
  , addingATask
  ]

  where
    listingEmptyTasks :: TestTree
    listingEmptyTasks = testCase "listing empty tasks" $ do
      ts <- extractTasks (H.listTasks)
      assertEqual "expecting no tasks" 0 (length ts)

    addingATask :: TestTree
    addingATask = testCase "adding one task" $ do
      ts <- extractTasks (H.addTask "some task" >> H.listTasks)
      assertEqual "expecting one task" 1 (length ts)

