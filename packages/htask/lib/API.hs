{-# LANGUAGE FlexibleInstances #-}

module API
  where

import Lib
import qualified Data.Text as Text
import Control.Monad.State
import Capabilities.Logging
import Event


addTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m, CanLog m)
  => Text.Text -> m (Either TaskError TaskRef)
addTask t = do
  event <- wrapEventType (TaskAdded t)
  appendEvent event
  applyEventToTasks event


startTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m, CanLog m)
  => TaskRef -> m (Either TaskError TaskRef)
startTask ref = do
  event <- wrapEventType (TaskStarted ref)
  appendEvent event
  applyEventToTasks event


completeTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m, CanLog m)
  => TaskRef -> m (Either TaskError TaskRef)
completeTask ref = do
  event <- wrapEventType (TaskCompleted ref)
  appendEvent event
  applyEventToTasks event


deleteTask
  :: (HasTasks m, CanCreateEvent m, CanStoreEvent m, CanLog m)
  => TaskRef -> m (Either TaskError TaskRef)
deleteTask ref = do
  event <- wrapEventType (TaskDeleted ref)
  appendEvent event
  applyEventToTasks event


listTasks :: (HasTasks m) => m Tasks
listTasks = getTasks
