{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.Core.Replay
  ( replayEventLog
  , applyRawEvent
  ) where

import qualified Events                   as V
import qualified HTask.Core.Task          as H
import qualified HTask.Core.TaskContainer as HC
import qualified HTask.Core.TaskEvent     as TV


replayEventLog
  :: (Monad m, HC.HasTasks m, Foldable f)
  => f TV.TaskEvent -> m ()
replayEventLog = mapM_ applyRawEvent


applyRawEvent
  :: (Monad m, HC.HasTasks m)
  => TV.TaskEvent -> m ()
applyRawEvent ev = do
  let td = V.payload ev
  case TV.intent td of
    (TV.AddTask text) -> do
      let t = H.Task (TV.detailRef td) text (V.timestamp ev) H.Pending
      _p <- HC.addNewTask t
      pure ()

    (TV.StartTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.InProgress
      pure ()

    (TV.StopTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.Pending
      pure ()

    (TV.CompleteTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.Complete
      pure ()

    (TV.RemoveTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.Abandoned
      pure ()
