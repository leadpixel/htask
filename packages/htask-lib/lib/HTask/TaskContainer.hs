{-# LANGUAGE FlexibleInstances #-}

module HTask.TaskContainer
  ( HasTasks (..)
  , Tasks
  , emptyTasks
  ) where

import qualified Control.Monad.State as State
import           HTask.Task

import           Data.List           (find)
import           Data.Maybe          (isNothing)


type Tasks = [Task]


class HasTasks m where
  getTasks :: m Tasks
  addNewTask :: Task -> m Bool
  updateExistingTask :: TaskRef -> (Task -> Task) -> m (Maybe Task)
  removeTaskRef :: TaskRef -> m Bool


instance (Monad m) => HasTasks (State.StateT Tasks m) where
  getTasks = State.get

  addNewTask t = do
    ts <- getTasks
    let p = findTask ts (taskRef t)
    if isNothing p
       then do
         State.put (t : ts)
         pure True
       else pure False

  updateExistingTask ref op = do
    ts <- getTasks
    maybe
      (pure Nothing)
      (\t -> do
        let t' = op t
        State.put (t' : removeTaskByRef ref ts)
        pure $ Just t')
      (findTask ts ref)

  removeTaskRef ref = do
    ts <- getTasks
    maybe
      (pure False)
      (\_t -> do
        State.put (removeTaskByRef ref ts)
        pure True)
      (findTask ts ref)


removeTaskByRef :: TaskRef -> Tasks -> Tasks
removeTaskByRef ref = filter (\k -> taskRef k /= ref)


emptyTasks :: Tasks
emptyTasks = mempty


findTask :: Tasks -> TaskRef -> Maybe Task
findTask ts ref = find matchesRef ts
  where
    matchesRef t = taskRef t == ref
