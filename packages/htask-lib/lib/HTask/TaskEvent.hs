{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.TaskEvent
  ( TaskEvent
  , TaskEventDetail (..)
  , TaskIntent (..)
  , applyIntentToTasks
  ) where

import qualified Events              as V
import qualified HTask.Task          as H
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
