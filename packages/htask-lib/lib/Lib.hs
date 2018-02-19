{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( TaskEvent
  , TaskError
  , CanStoreEvent (..)
  , TaskEventDetail (..)
  , TaskIntent (..)
  , applyIntentToTasks
  , replayEventLog
  ) where

import Data.Aeson
import GHC.Generics
import HTask.Event
import HTask.TaskContainer
import HTask.Task
import Control.Monad
import qualified Control.Monad.Writer as Writer
import qualified Data.Text as Text


data TaskEventDetail = TaskEventDetail
  { detailRef :: TaskRef
  , intent :: TaskIntent
  } deriving (Show, Generic)


type TaskEvent = Event TaskEventDetail
type EventLog = [TaskEvent]
type TaskError = String


data TaskIntent
  = AddTask Text.Text
  | StartTask TaskRef
  | StopTask TaskRef
  | CompleteTask TaskRef
  | DeleteTask TaskRef
  deriving (Show, Generic)

instance ToJSON TaskIntent
instance ToJSON TaskEventDetail
instance FromJSON TaskIntent
instance FromJSON TaskEventDetail


class CanStoreEvent m where
  appendEvent :: TaskEvent -> m ()

instance (Monad m) => CanStoreEvent (Writer.WriterT EventLog m) where
  appendEvent x = Writer.tell [x]


replayEventLog
  :: (Monad m, HasTasks m)
  => [TaskEvent] -> m ()
replayEventLog vs = do
  ts <- getTasks
  ks <- foldM (flip applyRawEvent) ts vs
  putTasks ks


applyRawEvent
  :: (Monad m, HasTasks m)
  => TaskEvent -> a -> m Tasks
applyRawEvent ev _ = do
  let td = eventType ev
  case intent td of
    (AddTask text) -> do
      let t = Task (detailRef td) text (timestamp ev) Pending
      _p <- addNewTask t
      getTasks

    (StartTask ref) -> do
      _p <- updateExistingTask ref $ setTaskStatus InProgress
      getTasks

    (StopTask ref) -> do
      _p <- updateExistingTask ref $ setTaskStatus Pending
      getTasks

    (CompleteTask ref) -> do
      _p <- updateExistingTask ref $ setTaskStatus Complete
      getTasks

    (DeleteTask ref) -> do
      _p <- removeTask ref
      getTasks



applyIntentToTasks
  :: (Monad m, CanCreateTask m, HasTasks m)
  => TaskIntent -> m (Either TaskError TaskEventDetail)
applyIntentToTasks itx =
  case itx of
    (AddTask text) -> do
      t <- createTask text
      p <- addNewTask t
      pure $ if p
        then Right (TaskEventDetail (taskRef t) itx)
        else Left "could not add; non-unique id"

    (StartTask ref) -> do
      p <- updateExistingTask ref $ setTaskStatus InProgress
      pure $ if p
        then Right (TaskEventDetail ref itx)
        else Left "could not find matching id"

    (StopTask ref) -> do
      p <- updateExistingTask ref $ setTaskStatus Pending
      pure $ if p
        then Right (TaskEventDetail ref itx)
        else Left "could not find matching id"

    (CompleteTask ref) -> do
      p <- updateExistingTask ref $ setTaskStatus Complete
      pure $ if p
        then Right (TaskEventDetail ref itx)
        else Left "could not find matching id"

    (DeleteTask ref) -> do
      p <- removeTask ref
      pure $ if p
        then Right (TaskEventDetail ref itx)
        else Left "unknown fuckup"

