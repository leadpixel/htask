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
  , CanRunTask
  , runTask
  ) where

import qualified Control.Monad.State as S
import qualified Control.Monad.Trans as T
import qualified Effects             as F
import qualified Events              as V
import qualified HTask.TaskContainer as HC
import qualified Replay              (replayEventLog)


type HasEventBackend m = (Monad m, V.HasEventSource m, V.HasEventSink m)
type CanRunTask m = (Monad m, HasEventBackend m)


newtype TaskApplication m a = TaskApp
  { runTaskApp :: S.StateT HC.Tasks m a
  } deriving (Functor, Applicative, Monad, T.MonadTrans)

instance (Monad m) => HC.HasTasks (TaskApplication m) where
  getTasks = TaskApp HC.getTasks
  addNewTask = TaskApp . HC.addNewTask
  updateExistingTask ref = TaskApp . HC.updateExistingTask ref
  removeTaskRef = TaskApp . HC.removeTaskRef

instance (Monad m, F.CanTime m) => F.CanTime (TaskApplication m) where
  now = T.lift F.now

instance (Monad m, F.CanUuid m) => F.CanUuid (TaskApplication m) where
  uuidGen = T.lift F.uuidGen

instance (Monad m, F.CanRandom m) => F.CanRandom (TaskApplication m) where
  getRandomRange = T.lift . F.getRandomRange

instance (Monad m, V.HasEventSink m) => V.HasEventSink (TaskApplication m) where
  writeEvent = TaskApp . T.lift . V.writeEvent


runTask
  :: (Monad m, V.HasEventSource m)
  => TaskApplication m a -> m a
runTask op
  = V.readEvents
  >>= \vs -> S.evalStateT (Replay.replayEventLog vs >> runTaskApp op) HC.emptyTasks
