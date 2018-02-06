{-# LANGUAGE FlexibleInstances #-}

module API
  where

import Lib
import qualified Data.Text as Text
import Control.Monad.State



addTask :: (HasTasks m, CanWriteTask m, CanLog m) => Text.Text -> m TaskRef
addTask t = do
  tasks <- getTasks
  event <- promote (TaskAdd t)
  tasks' <- parseEvents tasks [event]
  putTasks tasks'
  pure (eventUuid event)


startTask :: (HasTasks m, CanWriteTask m, CanLog m) => TaskRef -> m ()
startTask ref = do
  tasks <- getTasks
  event <- promote (TaskStart ref)
  tasks' <- parseEvents tasks [event]
  putTasks tasks'
  pure ()


listTasks :: (HasTasks m, CanLog m) => m Tasks
listTasks = getTasks
