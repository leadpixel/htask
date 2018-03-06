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
import HTask.Event
import HTask.TaskContainer
import HTask.Task


maybeStore
  :: (CanCreateEvent m, CanStoreEvent m)
  => Either TaskError TaskEventDetail -> m (Either TaskError TaskRef)
maybeStore r
  = case r of
      Left e -> pure (Left e)
      Right v -> Right <$> funk v


funk
  :: (CanCreateEvent m, CanStoreEvent m)
  => TaskEventDetail -> m TaskRef
funk v = do
  createEvent v >>= appendEvent
  pure (detailRef v)


addTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m)
  => Text.Text -> m (Either TaskError TaskRef)
addTask t = do
  r <- applyIntentToTasks (AddTask t)
  case r of
    Left e -> pure (Left e)
    Right v -> maybeStore r


startTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m)
  => TaskRef -> m (Either TaskError TaskRef)
startTask ref = do
  r <- applyIntentToTasks (StartTask ref)
  maybeStore r


stopTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m)
  => TaskRef -> m (Either TaskError TaskRef)
stopTask ref = do
  r <- applyIntentToTasks (StopTask ref)
  maybeStore r


completeTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m)
  => TaskRef -> m (Either TaskError TaskRef)
completeTask ref = do
  r <- applyIntentToTasks (CompleteTask ref)
  maybeStore r


removeTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m)
  => TaskRef -> m (Either TaskError TaskRef)
removeTask ref = do
  r <- applyIntentToTasks (RemoveTask ref)
  maybeStore r


listTasks :: (HasTasks m) => m Tasks
listTasks = getTasks
