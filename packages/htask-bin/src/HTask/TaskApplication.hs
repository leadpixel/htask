{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module HTask.TaskApplication
  ( TaskApplication
  , HasEventBackend
  , runTask
  ) where

import qualified Control.Monad.State as S
import qualified Control.Monad.Trans as T
import qualified Event               as V
import qualified HTask.TaskContainer as HC
import qualified Lib


type HasEventBackend m = (Monad m, V.HasEventSource m, V.HasEventSink m)


newtype TaskApplication m a = TaskApp
  { runTaskApp :: S.StateT HC.Tasks m a
  } deriving (Functor, Applicative, Monad, T.MonadTrans)

instance (Monad m) => HC.HasTasks (TaskApplication m) where
  getTasks = TaskApp HC.getTasks
  addNewTask = TaskApp . HC.addNewTask
  updateExistingTask ref = TaskApp . HC.updateExistingTask ref
  removeTaskRef = TaskApp . HC.removeTaskRef

instance (Monad m, V.CanTime m) => V.CanTime (TaskApplication m) where
  now = T.lift V.now

instance (Monad m, V.CanUuid m) => V.CanUuid (TaskApplication m) where
  uuidGen = T.lift V.uuidGen

instance (Monad m, V.HasEventSink m) => V.HasEventSink (TaskApplication m) where
  writeEvent = TaskApp . T.lift . V.writeEvent


runTask
  :: (Monad m, V.HasEventSource m)
  => TaskApplication m a -> m a
runTask op
  = V.readEvents
  >>= \vs -> S.evalStateT (Lib.replayEventLog vs >> runTaskApp op) HC.emptyTasks
