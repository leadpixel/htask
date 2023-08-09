{-# LANGUAGE FlexibleInstances #-}

module HTask.Core.TaskContainer (HasTasks (..)) where

import qualified Control.Monad.Trans.State as State
import qualified Data.Map.Strict           as Map
import qualified Data.Sequence             as Seq

import           Control.Monad.Trans.State (StateT)
import           Data.List                 (find)
import           Data.Map.Strict
import           Data.Maybe                (isNothing)
import           Data.Sequence             (Seq, (<|))
import           HTask.Core.Task


removeTaskByRef :: TaskUuid -> Seq Task -> Seq Task
removeTaskByRef ref = Seq.filter (\k -> taskUuid k /= ref)


findTask :: Seq Task -> TaskUuid -> Maybe Task
findTask ts ref = find matchesRef ts
  where
    matchesRef t = taskUuid t == ref


-- TODO: change Seq to Map TaskUuid Task
class HasTasks m where
  getTasks :: m (Seq Task)
  addNewTask :: Task -> m Bool
  updateExistingTask :: TaskUuid -> (Task -> Task) -> m (Maybe Task)
  removeTaskUuid :: TaskUuid -> m Bool


instance (Monad m) => HasTasks (StateT (Seq Task) m) where
  getTasks = stateGetTasks
  addNewTask = stateAddNewTask
  updateExistingTask = stateUpdateTask
  removeTaskUuid = stateRemoveTask


stateGetTasks :: (Monad m) => StateT (Seq Task) m (Seq Task)
stateGetTasks = State.get


stateAddNewTask :: (Monad m) => Task -> StateT (Seq Task) m Bool
stateAddNewTask t = do
  ts <- State.get
  let p = findTask ts (taskUuid t)
  if isNothing p
      then do
        State.put (t <| ts)
        pure True
      else pure False


stateUpdateTask :: (Monad m) => TaskUuid -> (Task -> Task) -> StateT (Seq Task) m (Maybe Task)
stateUpdateTask ref op = do
  ts <- State.get
  maybe
    (pure Nothing)
    (\t -> do
      let t' = op t
      State.put (t' <| removeTaskByRef ref ts)
      pure $ Just t')
    (findTask ts ref)


stateRemoveTask :: (Monad m) => TaskUuid -> StateT (Seq Task) m Bool
stateRemoveTask ref = do
  ts <- State.get
  maybe
    (pure False)
    (\_t -> do
      State.put (removeTaskByRef ref ts)
      pure True)
    (findTask ts ref)



instance (Monad m) => HasTasks (StateT (Map TaskUuid Task) m) where
  getTasks = do
    Seq.fromList . Map.elems <$> State.get

  addNewTask t = do
    ts <- State.get
    if member (taskUuid t) ts
        then pure False
        else do
          State.put $ Map.insert (taskUuid t) t ts
          pure True

  updateExistingTask ref op = do
    ts <- State.get
    maybe
      (pure Nothing)
      (\t -> do
        let t' = op t
        State.put $ Map.insert (taskUuid t) t' ts
        pure $ Just t')
      (Map.lookup ref ts)

  removeTaskUuid ref = do
    ts <- State.get
    maybe
      (pure False)
      (\_t -> do
        State.put (Map.delete ref ts)
        pure True)
      (Map.lookup ref ts)
