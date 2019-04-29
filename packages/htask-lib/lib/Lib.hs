{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( TaskEvent
  , TaskEventDetail (..)
  , TaskIntent (..)
  , applyIntentToTasks
  , replayEventLog
  ) where

import           Data.Aeson
import           Data.Text           (Text)
import           Event
import           GHC.Generics
import           HTask.Task
import           HTask.TaskContainer


data TaskEventDetail = TaskEventDetail
  { detailRef :: TaskRef
  , intent    :: TaskIntent
  } deriving (Show, Generic)


type TaskEvent = Event TaskEventDetail


data TaskIntent
  = AddTask Text
  | StartTask TaskRef
  | StopTask TaskRef
  | CompleteTask TaskRef
  | RemoveTask TaskRef
  deriving (Show, Eq, Generic)

instance ToJSON TaskIntent
instance ToJSON TaskEventDetail
instance FromJSON TaskIntent
instance FromJSON TaskEventDetail


replayEventLog
  :: (Monad m, HasTasks m, Foldable f)
  => f TaskEvent -> m ()
replayEventLog
  = mapM_ applyRawEvent


applyRawEvent
  :: (Monad m, HasTasks m)
  => TaskEvent -> m ()
applyRawEvent ev = do
  let td = payload ev
  case intent td of
    (AddTask text) -> do
      let t = Task (detailRef td) text (timestamp ev) Pending
      _p <- addNewTask t
      pure ()

    (StartTask ref) -> do
      _p <- updateExistingTask ref $ setTaskStatus InProgress
      pure ()

    (StopTask ref) -> do
      _p <- updateExistingTask ref $ setTaskStatus Pending
      pure ()

    (CompleteTask ref) -> do
      _p <- updateExistingTask ref $ setTaskStatus Complete
      pure ()

    (RemoveTask ref) -> do
      _p <- updateExistingTask ref $ setTaskStatus Abandoned
      pure ()



applyIntentToTasks
  :: (Monad m, CanCreateTask m, HasTasks m)
  => TaskIntent -> m (Either String TaskEventDetail)
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

    (RemoveTask ref) -> do
      p <- removeTaskRef ref
      pure $ if p
        then Right (TaskEventDetail ref itx)
        else Left "unknown fuckup"

