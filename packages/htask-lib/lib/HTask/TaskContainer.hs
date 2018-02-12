{-# LANGUAGE FlexibleInstances #-}

module HTask.TaskContainer
  ( HasTasks (..)
  , Tasks
  , emptyTasks
  ) where

import Data.Maybe
import Data.List
import HTask.Task
import Conduit
import qualified Control.Monad.State as State


type Tasks = [Task]


class HasTasks m where
  getTasks :: m Tasks
  putTasks :: Tasks -> m ()

  addNewTask :: Task -> m Bool
  updateExistingTask :: TaskRef -> (Task -> Task) -> m Bool
  removeTask :: TaskRef -> m Bool


instance (Monad m) => HasTasks (State.StateT Tasks m) where
  getTasks = State.get
  putTasks = State.put

  addNewTask t = do
    ts <- getTasks
    let p = findTask ts (taskRef t)
    if isNothing p
       then do
         putTasks (t : ts)
         pure True
       else pure False

  updateExistingTask ref op = do
    ts <- getTasks
    maybe
      (pure False)
      (\t -> do
        let k = op t
        putTasks (k : removeRef ts ref)
        pure True)
      (findTask ts ref)

  removeTask ref = do
    ts <- getTasks
    maybe
      (pure False)
      (\_t -> do
        putTasks (removeRef ts ref)
        pure True)
      (findTask ts ref)


instance (Monad m, MonadTrans t) => HasTasks (t (State.StateT Tasks m)) where
  getTasks = lift getTasks
  putTasks = lift . putTasks
  addNewTask = lift . addNewTask
  updateExistingTask ref = lift . updateExistingTask ref
  removeTask = lift . removeTask


removeRef :: Tasks -> TaskRef -> Tasks
removeRef ts ref = filter (\k -> taskRef k /= ref) ts


emptyTasks :: Tasks
emptyTasks = mempty


findTask :: Tasks -> TaskRef -> Maybe Task
findTask ts ref = find matchesRef ts
  where
    matchesRef t = taskRef t == ref
