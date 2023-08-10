{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.Core.TaskContainer
  ( TaskMap
  , addNewTask
  , getTasks
  , removeTaskUuid
  , updateExistingTask
  ) where

import qualified Control.Monad.State as State
import qualified Data.Map.Strict     as Map

import           Control.Monad.State (MonadState)
import           Data.Map.Strict     (Map)
import           HTask.Core.Task


type TaskMap = Map TaskUuid Task


getTasks :: (MonadState TaskMap m) => m TaskMap
getTasks = State.get


addNewTask :: (MonadState TaskMap m) => Task -> m Bool
addNewTask t = do
  ts <- State.get
  let (ts', p) = tryInsertTask t ts
  if p
      then do
        State.put ts'
        pure p
      else pure p

  where
    tryInsertTask :: Task -> Map TaskUuid Task -> (Map TaskUuid Task, Bool)
    tryInsertTask t' xs =
        if Map.member (taskUuid t') xs
            then (xs, False)
            else do
              (Map.insert (taskUuid t') t' xs, True)


updateExistingTask :: (MonadState TaskMap m) => TaskUuid -> (Task -> Task) -> m (Maybe Task)
updateExistingTask ref op = do
  ts <- State.get
  maybe
    (pure Nothing)
    (\t -> do
      let t' = op t
      State.put $ Map.insert (taskUuid t) t' ts
      pure $ Just t')
    (Map.lookup ref ts)


removeTaskUuid :: (MonadState TaskMap m) => TaskUuid -> m Bool
removeTaskUuid ref = do
  ts <- State.get
  maybe
    (pure False)
    (\_t -> do
      State.put (Map.delete ref ts)
      pure True)
    (Map.lookup ref ts)


-- Todo: move tryUpdateTask here
