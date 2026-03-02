module HTask.Core
  ( AddResult (..)
  , ModifyResult (..)
  , Task (createdAt, description, status, taskUuid)
  , TaskMap
  , TaskStatus (..)
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
import           HTask.Core.TaskContainer (TaskMap)
