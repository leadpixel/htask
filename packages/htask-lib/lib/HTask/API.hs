module HTask.API
  ( addTask
  , startTask
  , stopTask
  , completeTask
  , deleteTask
  , listTasks
  ) where

import Lib
import qualified Data.Text as Text
import HTask.Event
import HTask.TaskContainer
import HTask.Task


addTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m)
  => Text.Text -> m (Either TaskError TaskRef)
addTask t = do
  r <- applyIntentToTasks (AddTask t)
  maybeStore r


maybeStore
  :: (CanCreateEvent m, CanStoreEvent m)
  => Either TaskError TaskEventDetail -> m (Either TaskError TaskRef)
maybeStore r
  = case r of
      Left e -> pure (Left e)
      Right v -> do
        k <- createEvent v
        appendEvent k
        pure (Right $ detailRef v)


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


deleteTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m)
  => TaskRef -> m (Either TaskError TaskRef)
deleteTask ref = do
  r <- applyIntentToTasks (DeleteTask ref)
  maybeStore r


listTasks :: (HasTasks m) => m Tasks
listTasks = getTasks
