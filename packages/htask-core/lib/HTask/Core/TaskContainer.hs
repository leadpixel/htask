{-# LANGUAGE FlexibleInstances #-}

module HTask.Core.TaskContainer
  ( HasTasks (..)
  , Tasks
  , emptyTasks
  ) where

import qualified Control.Monad.Trans.State as State
import           HTask.Core.Task

import           Control.Monad.Trans.State (StateT)
import           Data.List                 (find)
import           Data.Maybe                (isNothing)


type Tasks = [Task]


emptyTasks :: Tasks
emptyTasks = mempty


removeTaskByRef :: TaskUuid -> Tasks -> Tasks
removeTaskByRef ref = filter (\k -> taskUuid k /= ref)


findTask :: Tasks -> TaskUuid -> Maybe Task
findTask ts ref = find matchesRef ts
  where
    matchesRef t = taskUuid t == ref


class HasTasks m where
  getTasks :: m Tasks
  addNewTask :: Task -> m Bool
  updateExistingTask :: TaskUuid -> (Task -> Task) -> m (Maybe Task)
  removeTaskUuid :: TaskUuid -> m Bool


instance (Monad m) => HasTasks (StateT Tasks m) where
  getTasks = stateGetTasks
  addNewTask = stateAddNewTask
  updateExistingTask = stateUpdateTask
  removeTaskUuid = stateRemoveTask


stateGetTasks :: (Monad m) => StateT Tasks m Tasks
stateGetTasks = State.get


stateAddNewTask :: (Monad m) => Task -> StateT Tasks m Bool
stateAddNewTask t = do
  ts <- getTasks
  let p = findTask ts (taskUuid t)
  if isNothing p
      then do
        State.put (t : ts)
        pure True
      else pure False


stateUpdateTask :: (Monad m) => TaskUuid -> (Task -> Task) -> StateT Tasks m (Maybe Task)
stateUpdateTask ref op = do
  ts <- getTasks
  maybe
    (pure Nothing)
    (\t -> do
      let t' = op t
      State.put (t' : removeTaskByRef ref ts)
      pure $ Just t')
    (findTask ts ref)


stateRemoveTask :: (Monad m) => TaskUuid -> StateT Tasks m Bool
stateRemoveTask ref = do
  ts <- getTasks
  maybe
    (pure False)
    (\_t -> do
      State.put (removeTaskByRef ref ts)
      pure True)
    (findTask ts ref)
