module HTask.Core
  ( AddResult (..)
  , CanAddTask
  , CanCreateTask
  , CanModifyTask
  , HasTasks
  , ModifyResult (..)
  , Task (..)
  , TaskEvent
  , TaskEventDetail (..)
  , TaskIntent (..)
  , TaskStatus (..)
  , addTask
  , completeTask
  , listTasks
  , removeTask
  , replayEventLog
  , startTask
  , stopTask
  , taskUuidToText
  ) where

import           HTask.Core.API
import           HTask.Core.Replay
import           HTask.Core.Task
import           HTask.Core.TaskContainer
import           HTask.Core.TaskEvent
