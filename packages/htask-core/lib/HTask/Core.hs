module HTask.Core
  ( AddResult (..)
  , CanCreateTask
  , HasTasks
  , ModifyResult (..)
    -- TODO: remove constructor export
  , Task (..)
  , TaskEvent
  , TaskIntent (..)
  , TaskStatus (..)
  , TaskUuid
  , addTask
  , completeTask
  , foldEventLog
  , listTasks
  , removeTask
  , startTask
  , stopTask
  , taskUuidToText
  ) where

import           HTask.Core.API
import           HTask.Core.Replay
import           HTask.Core.Task
import           HTask.Core.TaskContainer
import           HTask.Core.TaskEvent
