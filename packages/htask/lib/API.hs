{-# LANGUAGE FlexibleInstances #-}

module API
  where

import Lib
import qualified Data.Text as Text
import Control.Monad.State
import Capabilities.Logging


addTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m, CanLog m)
  => Text.Text -> m (Either TaskError TaskRef)
addTask t = do
  event <- wrapEventType (TaskAdd t)
  appendEvent event

  task <- applyEventToTasks event
  pure (taskRef <$> task)


startTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m, CanLog m)
  => TaskRef -> m (Either TaskError TaskRef)
startTask ref = do
  event <- wrapEventType (TaskStart ref)
  appendEvent event

  task <- applyEventToTasks event
  pure (taskRef <$> task)


listTasks :: (HasTasks m, CanLog m) => m Tasks
listTasks = getTasks
