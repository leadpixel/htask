{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTask.Core.API
  ( AddResult (..)
  , ModifyResult (..)
  , CanAddTask
  , CanModifyTask
  , addTask
  , findTask
  , startTask
  , stopTask
  , completeTask
  , removeTask
  , listTasks
  ) where

import qualified Data.Text                as Text
import qualified Data.Time                as Time
import qualified Events                   as V
import qualified HTask.Core.Task          as H
import qualified HTask.Core.TaskContainer as HC
import qualified HTask.Core.TaskEvent     as TV

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Functor             (($>))
import           Data.List
import           Data.Text                (Text)
import           Data.Time                (UTCTime)


data AddResult
  = AddSuccess H.TaskRef
  | FailedToAdd
  deriving (Show, Eq)


data ModifyResult
  = ModifySuccess H.Task
  | FailedToModify
  | FailedToFind
  deriving (Show, Eq)


type CanAddTask m = (Monad m, V.HasEventSink m, HC.HasTasks m, H.CanCreateTask m)
type CanModifyTask m = (Monad m, V.HasEventSink m, HC.HasTasks m)


readTime :: (MonadIO m) => m UTCTime
readTime = liftIO Time.getCurrentTime


addTask
  :: (CanAddTask m, MonadIO m)
  => Text -> m AddResult
addTask tx = do
  tk <- H.createTask tx
  p <- HC.addNewTask tk

  if p
    then
      (V.createEvent readTime (TV.TaskEventDetail (H.taskRef tk) (TV.AddTask tx)) >>= V.writeEvent) $> AddSuccess (H.taskRef tk)

    else
      pure FailedToAdd


findTask :: (MonadIO m, HC.HasTasks m) => Text -> m (Maybe H.Task)
findTask tx
  = find (uuidStartsWith tx)
  <$> listTasks

  where
    uuidStartsWith :: Text -> H.Task -> Bool
    uuidStartsWith t
      = Text.isPrefixOf t . H.taskRefText . H.taskRef


withMatch :: (MonadIO m, HC.HasTasks m) => Text -> (H.Task -> m ModifyResult) -> m ModifyResult
withMatch tx op
  = findTask tx >>= maybe (pure FailedToFind) op


startTask
  :: (CanModifyTask m, MonadIO m)
  => Text -> m ModifyResult
startTask tx =
  withMatch tx $ \tsk -> do
    let ref = H.taskRef tsk
    p <- HC.updateExistingTask ref $ H.setTaskStatus H.InProgress

    case p of
      Nothing ->
        pure FailedToModify

      Just t -> do
        x <- V.createEvent readTime (TV.TaskEventDetail ref (TV.StartTask ref))
        V.writeEvent x
        pure $ ModifySuccess t


stopTask
  :: (CanModifyTask m, MonadIO m)
  => Text -> m ModifyResult
stopTask tx =
  withMatch tx $ \tsk -> do
    let ref = H.taskRef tsk
    p <- HC.updateExistingTask ref $ H.setTaskStatus H.Pending

    case p of
      Nothing ->
        pure FailedToModify

      Just t -> do
        V.createEvent readTime (TV.TaskEventDetail ref (TV.StopTask ref)) >>= V.writeEvent
        pure $ ModifySuccess t


completeTask
  :: (CanModifyTask m, MonadIO m)
  => Text -> m ModifyResult
completeTask tx =
  withMatch tx $ \tsk -> do
    let ref = H.taskRef tsk
    p <- HC.updateExistingTask ref $ H.setTaskStatus H.Complete

    case p of
      Nothing ->
        pure FailedToModify

      Just t -> do
        V.createEvent readTime (TV.TaskEventDetail ref (TV.CompleteTask ref)) >>= V.writeEvent
        pure $ ModifySuccess t


removeTask
  :: (CanModifyTask m, MonadIO m)
  => Text -> m ModifyResult
removeTask tx =
  withMatch tx $ \tsk -> do
    let ref = H.taskRef tsk
    p <- HC.removeTaskRef ref

    if p
      then do
        V.createEvent readTime (TV.TaskEventDetail ref (TV.RemoveTask ref)) >>= V.writeEvent
        pure $ ModifySuccess tsk

      else
        pure FailedToModify


listTasks :: (HC.HasTasks m) => m HC.Tasks
listTasks = HC.getTasks
