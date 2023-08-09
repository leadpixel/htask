{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.Core.Replay
  ( applyEvent
  , foldEventLog
  , replayEventLog
  ) where

import qualified Data.Map                 as Map
import qualified Data.Sequence            as Seq
import qualified Leadpixel.Events         as V

import           Control.Monad            (void)
import           Data.Foldable
import           Data.Map                 (Map)
import           Data.Sequence            (Seq)
import           HTask.Core.Task
import           HTask.Core.TaskContainer
import           HTask.Core.TaskEvent


replayEventLog
  :: (Monad m, HasTasks m, Foldable f)
  => f TaskEvent -> m (Seq Task)
replayEventLog k
  = mapM_ applyEvent k >> getTasks


foldEventLog
  :: (Foldable f)
  => f TaskEvent -> Seq Task
foldEventLog = Seq.fromList . Map.elems . foldl' f mempty
  where
    f :: Map TaskUuid Task -> TaskEvent -> Map TaskUuid Task
    f xs ev =
      let td = V.payload ev
      in case intent td of
        (AddTask text) ->
          let t = Task (detailRef td) text (V.timestamp ev) Pending
           in if Map.member (taskUuid t) xs
                then xs
                else Map.insert (taskUuid t) t xs

        (StartTask ref) ->
          updateExistingTask' ref (setTaskStatus InProgress) xs

        (StopTask ref) ->
          updateExistingTask' ref (setTaskStatus Pending) xs

        (CompleteTask ref) ->
          updateExistingTask' ref (setTaskStatus Complete) xs

        (RemoveTask ref) ->
          updateExistingTask' ref (setTaskStatus Abandoned) xs



updateExistingTask' ref op xs = do
  maybe
    xs
    (\t -> do
      let t' = op t
      Map.insert (taskUuid t) t' xs
      )
    (Map.lookup ref xs)



-- TODO: allow for failure
applyEvent
  :: (Monad m, HasTasks m)
  => TaskEvent -> m ()
applyEvent ev = do
  let td = V.payload ev
  case intent td of
    (AddTask text) ->
      void $ addNewTask (Task (detailRef td) text (V.timestamp ev) Pending)

    (StartTask ref) ->
      void $ updateExistingTask ref (setTaskStatus InProgress)

    (StopTask ref) ->
      void $ updateExistingTask ref (setTaskStatus Pending)

    (CompleteTask ref) ->
      void $ updateExistingTask ref (setTaskStatus Complete)

    (RemoveTask ref) ->
      void $ updateExistingTask ref (setTaskStatus Abandoned)
