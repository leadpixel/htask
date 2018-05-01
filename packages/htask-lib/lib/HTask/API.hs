{-# LANGUAGE ConstraintKinds          #-}

module HTask.API
  ( addTask
  , startTask
  , stopTask
  , completeTask
  , removeTask
  , listTasks
  ) where

import Lib
import qualified Data.Text as Text
import Event
import HTask.TaskContainer
import HTask.Task


maybeStore
  :: (CanCreateTask m, HasEventSink m)
  => Either String TaskEventDetail -> m (Either String TaskRef)
maybeStore r
  = case r of
      Left e -> pure (Left e)
      Right v -> Right <$> funk v


funk
  :: (CanCreateTask m, HasEventSink m)
  => TaskEventDetail -> m TaskRef
funk v = do
  createEvent v >>= writeEvent
  pure (detailRef v)



addTask
  :: (HasTasks m, CanCreateTask m, CanCreateEvent m, HasEventSink m)
  => Text.Text -> m (Either String TaskRef)
addTask tx = do

  -- create
  tk <- createTask tx
  let detail = TaskEventDetail (taskRef tk) (AddTask tx)
  e <- createEvent detail

  -- apply
  p <- addNewTask tk

  -- persist
  if p then writeEvent e else pure ()

  pure $ if p
            then Right (taskRef tk)
            else Left "could not add"


startTask
  :: (HasTasks m, CanCreateEvent m, HasEventSink m)
  => TaskRef -> m (Either String TaskRef)
startTask ref = do

  -- create
  let detail = TaskEventDetail ref (StartTask ref)
  e <- createEvent detail

  -- apply
  p <- updateExistingTask ref $ setTaskStatus InProgress

  if p then writeEvent e else pure ()

  pure $ if p
            then Right ref
            else Left "could not find matching id"


stopTask
  :: (HasTasks m, CanCreateTask m, HasEventSink m)
  => TaskRef -> m (Either String TaskRef)
stopTask ref = do
  r <- applyIntentToTasks (StopTask ref)
  maybeStore r


completeTask
  :: (HasTasks m, CanCreateTask m, HasEventSink m)
  => TaskRef -> m (Either String TaskRef)
completeTask ref = do
  r <- applyIntentToTasks (CompleteTask ref)
  maybeStore r


removeTask
  :: (HasTasks m, CanCreateTask m, HasEventSink m)
  => TaskRef -> m (Either String TaskRef)
removeTask ref = do
  r <- applyIntentToTasks (RemoveTask ref)
  maybeStore r


listTasks :: (HasTasks m) => m Tasks
listTasks = getTasks
