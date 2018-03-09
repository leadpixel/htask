{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.TaskApplication
  ( TaskApplication (..)
  , TaskConfig
  , runTask
  ) where

import qualified HTask as H
import HTask.Config
import Event
import Conduit
import Data.Aeson
import qualified Control.Monad.Reader   as R
import qualified Control.Monad.State    as S
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Maybe


type FileBackend = R.ReaderT FilePath IO


instance EventBackend FileBackend where
  readEvents = fileReadEvents
  writeEvent = fileWriteEvent


fileReadEvents :: (FromJSON a) => FileBackend [Event a]
fileReadEvents = R.ask >>= liftIO . y
  where
    y :: (FromJSON b) => FilePath -> IO [Event b]
    y = fmap (catMaybes . fmap (decode . UTF8.fromString) . lines) .  readFile


fileWriteEvent :: (ToJSON a) => Event a -> FileBackend ()
fileWriteEvent ev = R.ask >>= \z -> liftIO (t z ev)
  where
    t :: (ToJSON b) => FilePath -> b -> IO ()
    t f = BS.appendFile f
        . Lazy.toStrict
        . flip mappend "\n"
        . encode



type TaskConfig = R.ReaderT GlobalOptions IO


runWithFile :: FileBackend a -> TaskConfig a
runWithFile = R.withReaderT taskfile


newtype TaskApplication a = TaskApp
  { unwrapTaskApp :: S.StateT H.Tasks TaskConfig a
  } deriving (Functor, Applicative, Monad)


instance H.HasTasks TaskApplication where
  getTasks = TaskApp H.getTasks
  putTasks = TaskApp . H.putTasks
  addNewTask = TaskApp . H.addNewTask
  updateExistingTask ref = TaskApp . H.updateExistingTask ref
  removeTaskRef = TaskApp . H.removeTaskRef


instance CanTime TaskApplication where
  now = TaskApp $ lift $ lift now


instance CanUuid TaskApplication where
  uuidGen = TaskApp $ lift $ lift uuidGen


instance H.CanStoreEvent TaskApplication where
  appendEvent ev
    = TaskApp $ lift (runWithFile $ writeEvent ev)


prepTasks :: [H.TaskEvent] -> TaskConfig [H.Task]
prepTasks vs
  = S.execStateT
      (unwrapTaskApp $ H.replayEventLog vs)
      H.emptyTasks


runTask :: TaskApplication a -> TaskConfig a
runTask op
  = runWithFile readEvents
  >>= prepTasks
  >>= S.evalStateT (unwrapTaskApp op)
