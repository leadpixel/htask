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
  :: (CanCreateEvent m, HasEventSink m)
  => Either String TaskEventDetail -> m (Either String TaskRef)
maybeStore r
  = case r of
      Left e -> pure (Left e)
      Right v -> Right <$> funk v


funk
  :: (CanCreateEvent m, HasEventSink m)
  => TaskEventDetail -> m TaskRef
funk v = do
  createEvent v >>= writeEvent
  pure (detailRef v)


addTask
  :: (HasTasks m, CanCreateEvent m, HasEventSink m)
  => Text.Text -> m (Either String TaskRef)
addTask t = do
  r <- applyIntentToTasks (AddTask t)
  case r of
    Left e -> pure (Left e)
    Right _v -> maybeStore r


startTask
  :: (HasTasks m, CanCreateEvent m, HasEventSink m)
  => TaskRef -> m (Either String TaskRef)
startTask ref = do
  r <- applyIntentToTasks (StartTask ref)
  maybeStore r


stopTask
  :: (HasTasks m, CanCreateEvent m, HasEventSink m)
  => TaskRef -> m (Either String TaskRef)
stopTask ref = do
  r <- applyIntentToTasks (StopTask ref)
  maybeStore r


completeTask
  :: (HasTasks m, CanCreateEvent m, HasEventSink m)
  => TaskRef -> m (Either String TaskRef)
completeTask ref = do
  r <- applyIntentToTasks (CompleteTask ref)
  maybeStore r


removeTask
  :: (HasTasks m, CanCreateEvent m, HasEventSink m)
  => TaskRef -> m (Either String TaskRef)
removeTask ref = do
  r <- applyIntentToTasks (RemoveTask ref)
  maybeStore r


listTasks :: (HasTasks m) => m Tasks
listTasks = getTasks
