{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module HTask.CLI.TaskApplication
  ( TaskApplication
  , HasEventBackend
  , CanRunTask
  , runTask
  ) where

import qualified Control.Monad.State      as S
import qualified Control.Monad.Trans      as T
import qualified Events                   as V
import qualified HTask.Core.TaskContainer as HC

import           HTask.Core.Replay        (replayEventLog)
import           Leadpixel.Provider


type HasEventBackend m = (Monad m, V.HasEventSource m, V.HasEventSink m)
type CanRunTask m = (Monad m, HasEventBackend m)


newtype TaskApplication m a = TaskApp
  { unTaskApp :: S.StateT HC.Tasks m a
  } deriving (Functor, Applicative, Monad, T.MonadTrans)

instance (Monad m) => HC.HasTasks (TaskApplication m) where
  getTasks = TaskApp HC.getTasks
  addNewTask = TaskApp . HC.addNewTask
  updateExistingTask ref = TaskApp . HC.updateExistingTask ref
  removeTaskRef = TaskApp . HC.removeTaskRef

instance (Monad m, V.HasEventSink m) => V.HasEventSink (TaskApplication m) where
  writeEvent = T.lift . V.writeEvent

instance (Provider k m) => Provider k (TaskApplication m) where
  gen = T.lift gen


runTask
  :: (Monad m, V.HasEventSource m)
  => TaskApplication m a -> m a
runTask op
  = V.readEvents
  >>= \vs -> S.evalStateT (replayEventLog vs >> unTaskApp op) HC.emptyTasks
