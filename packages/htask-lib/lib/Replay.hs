{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}

module Replay
  ( replayEventLog
  ) where

import qualified Events              as V
import qualified HTask.Task          as H
import qualified HTask.TaskContainer as HC
import qualified Lib


replayEventLog
  :: (Monad m, HC.HasTasks m, Foldable f)
  => f Lib.TaskEvent -> m ()
replayEventLog = mapM_ applyRawEvent


applyRawEvent
  :: (Monad m, HC.HasTasks m)
  => Lib.TaskEvent -> m ()
applyRawEvent ev = do
  let td = V.payload ev
  case Lib.intent td of
    (Lib.AddTask text) -> do
      let t = H.Task (Lib.detailRef td) text (V.timestamp ev) H.Pending
      _p <- HC.addNewTask t
      pure ()

    (Lib.StartTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.InProgress
      pure ()

    (Lib.StopTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.Pending
      pure ()

    (Lib.CompleteTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.Complete
      pure ()

    (Lib.RemoveTask ref) -> do
      _p <- HC.updateExistingTask ref $ H.setTaskStatus H.Abandoned
      pure ()
