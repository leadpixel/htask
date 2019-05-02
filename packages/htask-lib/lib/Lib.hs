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

import qualified Event as V
import qualified HTask.Task as H
import qualified HTask.TaskContainer as HC

import           Data.Text           (Text)
import           GHC.Generics

import           Data.Aeson


data TaskIntent
  = AddTask Text
  | StartTask H.TaskRef
  | StopTask H.TaskRef
  | CompleteTask H.TaskRef
  | RemoveTask H.TaskRef
  deriving (Show, Eq, Generic)

instance ToJSON TaskIntent
instance FromJSON TaskIntent


data TaskEventDetail = TaskEventDetail
  { detailRef :: H.TaskRef
  , intent    :: TaskIntent
  } deriving (Show, Generic)

instance ToJSON TaskEventDetail
instance FromJSON TaskEventDetail


type TaskEvent = V.Event TaskEventDetail


replayEventLog
  :: (Monad m, HC.HasTasks m, Foldable f)
  => f TaskEvent -> m ()
replayEventLog = mapM_ applyRawEvent


applyRawEvent
  :: (Monad m, HC.HasTasks m)
  => TaskEvent -> m ()
applyRawEvent ev = do
  let td = V.payload ev
  case intent td of
    (AddTask text) -> do
      let t = H.Task (detailRef td) text (V.timestamp ev) H.Pending
      _p <- HC.addNewTask t
      pure ()

    (StartTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.InProgress
      pure ()

    (StopTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.Pending
      pure ()

    (CompleteTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.Complete
      pure ()

    (RemoveTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.Abandoned
      pure ()



applyIntentToTasks
  :: (Monad m, H.CanCreateTask m, HC.HasTasks m)
  => TaskIntent -> m (Either String TaskEventDetail)
applyIntentToTasks itx =
  case itx of
    (AddTask text) -> do
      t <- H.createTask text
      p <- HC.addNewTask t
      pure $ if p
        then Right (TaskEventDetail (H.taskRef t) itx)
        else Left "could not add; non-unique id"

    (StartTask ref) -> do
      p <- HC.updateExistingTask ref $ H.setTaskStatus H.InProgress
      pure $ if p
        then Right (TaskEventDetail ref itx)
        else Left "could not find matching id"

    (StopTask ref) -> do
      p <- HC.updateExistingTask ref $ H.setTaskStatus H.Pending
      pure $ if p
        then Right (TaskEventDetail ref itx)
        else Left "could not find matching id"

    (CompleteTask ref) -> do
      p <- HC.updateExistingTask ref $ H.setTaskStatus H.Complete
      pure $ if p
        then Right (TaskEventDetail ref itx)
        else Left "could not find matching id"

    (RemoveTask ref) -> do
      p <- HC.removeTaskRef ref
      pure $ if p
        then Right (TaskEventDetail ref itx)
        else Left "unknown fuckup"
