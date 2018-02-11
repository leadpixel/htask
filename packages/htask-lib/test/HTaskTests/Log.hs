{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HTaskTests.Log
  where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W
import qualified HTask as H


type LogTestMonad = W.WriterT H.EventLog (S.StateT H.Tasks IO)

instance H.CanTime LogTestMonad where
  now = W.lift (S.lift H.now)

instance H.CanUuid LogTestMonad where
  uuidGen = W.lift (S.lift H.uuidGen)


extractLog :: LogTestMonad a -> IO H.EventLog
extractLog op = do
  S.evalStateT
    (W.execWriterT op)
    H.emptyTasks


test_log :: TestTree
test_log = testGroup "logs"
  [ listingEmptyTasks
  , addingATask
  ]

  where
    listingEmptyTasks :: TestTree
    listingEmptyTasks = testCase "listing empty tasks" $ do
      log <- extractLog (H.listTasks)
      assertEqual "expecting no logs" 0 (length log)

    addingATask :: TestTree
    addingATask = testCase "adding one task" $ do
      log <- extractLog (H.addTask "some task")
      assertEqual "expecting one log entry" 1 (length log)

