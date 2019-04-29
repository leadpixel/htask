{-# LANGUAGE FlexibleInstances #-}

module HTask.TaskContainer
  ( HasTasks (..)
  , Tasks
  , emptyTasks
  ) where

import qualified Control.Monad.State as State
import           Data.List
import           Data.Maybe
import           HTask.Task


type Tasks = [Task]


class HasTasks m where
  getTasks :: m Tasks
  addNewTask :: Task -> m Bool
  updateExistingTask :: TaskRef -> (Task -> Task) -> m Bool
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
      (pure False)
      (\t -> do
        let k = op t
        State.put (k : removeTaskByRef ts ref)
        pure True)
      (findTask ts ref)

  removeTaskRef ref = do
    ts <- getTasks
    maybe
      (pure False)
      (\_t -> do
        State.put (removeTaskByRef ts ref)
        pure True)
      (findTask ts ref)


removeTaskByRef :: Tasks -> TaskRef -> Tasks
removeTaskByRef ts ref = filter (\k -> taskRef k /= ref) ts


emptyTasks :: Tasks
emptyTasks = mempty


findTask :: Tasks -> TaskRef -> Maybe Task
findTask ts ref = find matchesRef ts
  where
    matchesRef t = taskRef t == ref
