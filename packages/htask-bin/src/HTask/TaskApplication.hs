{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.TaskApplication
  ( TaskApplication (..)
  , TaskConfig
  , runTask
  ) where

import qualified HTask as H
import HTask.Config
import Conduit
import Data.Aeson
import qualified Control.Monad.Reader   as R
import qualified Control.Monad.State    as S
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Maybe


type TaskConfig = R.ReaderT GlobalOptions IO

newtype TaskApplication a = TaskApp
  { unwrapTaskApp :: S.StateT H.Tasks TaskConfig a
  } deriving (Functor, Applicative, Monad)


instance H.HasTasks TaskApplication where
  getTasks = TaskApp H.getTasks
  putTasks = TaskApp . H.putTasks
  addNewTask = TaskApp . H.addNewTask
  updateExistingTask ref = TaskApp . H.updateExistingTask ref
  removeTask = TaskApp . H.removeTask


instance H.CanTime TaskApplication where
  now = TaskApp $ lift $ lift H.now


instance H.CanUuid TaskApplication where
  uuidGen = TaskApp $ lift $ lift H.uuidGen


instance H.CanStoreEvent TaskApplication where
  appendEvent ev
    = TaskApp $ lift (R.ask >>= lift . flip k ev)

    where
      k :: GlobalOptions -> H.TaskEvent -> IO ()
      k opts
        = BS.appendFile (taskfile opts)
        . Lazy.toStrict
        . flip mappend "\n"
        . encode


readTaskEvents :: TaskConfig [H.TaskEvent]
readTaskEvents = R.ask >>= liftIO . k
  where
    k :: GlobalOptions -> IO [H.TaskEvent]
    k opts = (parselines . lines) <$> readFile (taskfile opts)


parselines :: [String] -> [H.TaskEvent]
parselines = catMaybes . fmap (decode . UTF8.fromString)


prepTasks :: [H.TaskEvent] -> TaskConfig [H.Task]
prepTasks vs
  = S.execStateT
      (unwrapTaskApp $ H.replayEventLog vs)
      H.emptyTasks


runTask :: TaskApplication a -> TaskConfig a
runTask op = do
  ts <- readTaskEvents >>= prepTasks
  S.evalStateT (unwrapTaskApp op) ts
