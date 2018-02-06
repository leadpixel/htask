{-# LANGUAGE FlexibleInstances #-}

module API
  where

import Lib
import qualified Data.Text as Text
import Control.Monad.State


class HasTasks m where
  getTasks :: m Tasks
  putTasks :: Tasks -> m ()

instance (Monad m) => HasTasks (StateT Tasks m) where
  getTasks = get
  putTasks = put


addTask :: (HasTasks m, CanWriteTask m) => Text.Text -> m TaskRef
addTask t = do
  tasks <- getTasks
  event <- promote (TaskAdd t)
  tasks' <- parseEvents tasks [event]
  putTasks tasks'
  pure (eventUuid event)


startTask :: (HasTasks m, CanWriteTask m) => TaskRef -> m ()
startTask ref = do
  tasks <- getTasks
  event <- promote (TaskStart ref)
  tasks' <- parseEvents tasks [event]
  putTasks tasks'
  pure ()
