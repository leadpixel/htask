{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.TaskApplication
  ( TaskApplication (..)
  , unwrapTaskApp
  , runTask
  ) where

import qualified HTask as H
import Conduit
import Data.Aeson
import qualified Control.Monad.State    as State
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.ByteString.Lazy.UTF8 as UTF8

import Data.Maybe

newtype TaskApplication a = TaskApp
  { unwrapTaskApp :: State.StateT H.Tasks IO a
  } deriving (Functor, Applicative, Monad)


instance H.HasTasks TaskApplication where
  getTasks = TaskApp H.getTasks
  putTasks = TaskApp . H.putTasks
  addNewTask = TaskApp . H.addNewTask
  updateExistingTask ref = TaskApp . H.updateExistingTask ref
  removeTask = TaskApp . H.removeTask


instance H.CanTime TaskApplication where
  now = TaskApp $ lift H.now


instance H.CanUuid TaskApplication where
  uuidGen = TaskApp $ lift H.uuidGen


instance H.CanStoreEvent TaskApplication where
  appendEvent
    = TaskApp
    . lift
    . BS.appendFile "tasks.txt"
    . Lazy.toStrict
    . flip mappend "\n"
    . encode


readTaskEvents :: FilePath -> IO [H.TaskEvent]
readTaskEvents p = (parseLines . lines) <$> readFile p
  where
    parseLines :: [String] -> [H.TaskEvent]
    parseLines = catMaybes . fmap (decode . UTF8.fromString)


prepTasks :: [H.TaskEvent] -> IO [H.Task]
prepTasks vs
  = State.execStateT
      (unwrapTaskApp $ H.replayEventLog vs)
      H.emptyTasks



--   vs <- readTaskEvents "tasks.txt"
--   xs <- prepTasks vs
--   ts <- runTask H.listTasks xs



runTask :: TaskApplication a -> FilePath -> IO a
runTask op file = do
  ts <- readTaskEvents file >>= prepTasks
  State.evalStateT (unwrapTaskApp op) ts
