{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.Core.Replay (replayEventLog) where

import qualified Leadpixel.Events         as V

import           Data.Sequence            (Seq)
import           HTask.Core.Task
import           HTask.Core.TaskContainer
import           HTask.Core.TaskEvent


replayEventLog
  :: (Monad m, HasTasks m, Foldable f)
  => f TaskEvent -> m (Seq Task)
replayEventLog k
  = mapM_ applyEvent k >> getTasks


applyEvent
  :: (Monad m, HasTasks m)
  => TaskEvent -> m ()
applyEvent ev = do
  let td = V.payload ev
  case intent td of
    (AddTask text) -> do
      let t = Task (detailRef td) text (V.timestamp ev) Pending
      _p <- addNewTask t
      pure ()

    (StartTask ref) -> do
      _p <- updateExistingTask ref $ setTaskStatus InProgress
      pure ()

    (StopTask ref) -> do
      _p <- updateExistingTask ref $ setTaskStatus Pending
      pure ()

    (CompleteTask ref) -> do
      _p <- updateExistingTask ref $ setTaskStatus Complete
      pure ()

    (RemoveTask ref) -> do
      _p <- updateExistingTask ref $ setTaskStatus Abandoned
      pure ()
