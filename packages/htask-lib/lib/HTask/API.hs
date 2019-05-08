{-# LANGUAGE ConstraintKinds #-}

module HTask.API
  ( addTask
  , startTask
  , stopTask
  , completeTask
  , removeTask
  , listTasks
  ) where

import qualified Events              as V
import qualified HTask.Task          as H
import qualified HTask.TaskContainer as HC
import qualified HTask.TaskEvent as TV

import           Data.Text           (Text)


maybeStore
  :: (H.CanCreateTask m, V.HasEventSink m)
  => Either String TV.TaskEventDetail -> m (Either String H.TaskRef)
maybeStore r
  = case r of
      Left e  -> pure (Left e)
      Right v -> Right <$> funk v


funk
  :: (H.CanCreateTask m, V.HasEventSink m)
  => TV.TaskEventDetail -> m H.TaskRef
funk v = do
  V.createEvent v >>= V.writeEvent
  pure (TV.detailRef v)



addTask
  :: (HC.HasTasks m, H.CanCreateTask m, V.CanCreateEvent m, V.HasEventSink m)
  => Text -> m (Either String H.TaskRef)
addTask tx = do

  -- create
  tk <- H.createTask tx
  let detail = TV.TaskEventDetail (H.taskRef tk) (TV.AddTask tx)
  e <- V.createEvent detail

  -- apply
  p <- HC.addNewTask tk

  -- persist
  if p then V.writeEvent e else pure ()

  pure $ if p
            then Right (H.taskRef tk)
            else Left "could not add"


startTask
  :: (Monad m, HC.HasTasks m, V.CanCreateEvent m, V.HasEventSink m)
  => H.TaskRef -> m (Either String H.TaskRef)
startTask ref = do

  -- create
  let detail = TV.TaskEventDetail ref (TV.StartTask ref)
  e <- V.createEvent detail

  -- apply
  p <- HC.updateExistingTask ref $ H.setTaskStatus H.InProgress

  if p then V.writeEvent e else pure ()

  pure $ if p
            then Right ref
            else Left "could not find matching id"


stopTask
  :: (HC.HasTasks m, H.CanCreateTask m, V.HasEventSink m)
  => H.TaskRef -> m (Either String H.TaskRef)
stopTask ref = do
  r <- TV.applyIntentToTasks (TV.StopTask ref)
  maybeStore r


completeTask
  :: (HC.HasTasks m, H.CanCreateTask m, V.HasEventSink m)
  => H.TaskRef -> m (Either String H.TaskRef)
completeTask ref = do
  r <- TV.applyIntentToTasks (TV.CompleteTask ref)
  maybeStore r


removeTask
  :: (HC.HasTasks m, H.CanCreateTask m, V.HasEventSink m)
  => H.TaskRef -> m (Either String H.TaskRef)
removeTask ref = do
  r <- TV.applyIntentToTasks (TV.RemoveTask ref)
  maybeStore r


listTasks :: (HC.HasTasks m) => m HC.Tasks
listTasks = HC.getTasks
