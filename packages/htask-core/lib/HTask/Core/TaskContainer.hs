{-# LANGUAGE FlexibleInstances #-}

module HTask.Core.TaskContainer
  ( HasTasks (..)
  ) where

import qualified Control.Monad.Trans.State as State
import qualified Data.Sequence             as Seq

import           Control.Monad.Trans.State (StateT)
import           Data.List                 (find)
import           Data.Maybe                (isNothing)
import           Data.Sequence             (Seq, (<|), (|>))
import           HTask.Core.Task


removeTaskByRef :: TaskUuid -> Seq Task -> Seq Task
removeTaskByRef ref = Seq.filter (\k -> taskUuid k /= ref)


findTask :: Seq Task -> TaskUuid -> Maybe Task
findTask ts ref = find matchesRef ts
  where
    matchesRef t = taskUuid t == ref


class HasTasks m where
  getTasks :: m (Seq Task)
  addNewTask :: Task -> m Bool
  updateExistingTask :: TaskUuid -> (Task -> Task) -> m (Maybe Task)
  removeTaskUuid :: TaskUuid -> m Bool


instance (Monad m) => HasTasks (StateT (Seq Task) m) where
  getTasks = stateGetTask
  addNewTask = stateAddNewTask
  updateExistingTask = stateUpdateTask
  removeTaskUuid = stateRemoveTask


stateGetTask :: (Monad m) => StateT (Seq Task) m (Seq Task)
stateGetTask = State.get


stateAddNewTask :: (Monad m) => Task -> StateT (Seq Task) m Bool
stateAddNewTask t = do
  ts <- getTasks
  let p = findTask ts (taskUuid t)
  if isNothing p
      then do
        State.put (t <| ts)
        pure True
      else pure False


stateUpdateTask :: (Monad m) => TaskUuid -> (Task -> Task) -> StateT (Seq Task) m (Maybe Task)
stateUpdateTask ref op = do
  ts <- getTasks
  maybe
    (pure Nothing)
    (\t -> do
      let t' = op t
      State.put (t' <| removeTaskByRef ref ts)
      pure $ Just t')
    (findTask ts ref)


stateRemoveTask :: (Monad m) => TaskUuid -> StateT (Seq Task) m Bool
stateRemoveTask ref = do
  ts <- getTasks
  maybe
    (pure False)
    (\_t -> do
      State.put (removeTaskByRef ref ts)
      pure True)
    (findTask ts ref)
