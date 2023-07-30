{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HTask.CLI.TaskApplication
  ( TaskApplication
  , HasEventBackend
  , CanRunTask
  , runTask
  ) where

import qualified Events                    as V
import qualified HTask.Core.TaskContainer  as HC

import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.State (StateT, evalStateT)
import           HTask.Core.Replay         (replayEventLog)
import           Leadpixel.Provider


type HasEventBackend m = (MonadIO m, V.HasEventSource m, V.HasEventSink m)
type CanRunTask m = (Monad m, HasEventBackend m)


newtype TaskApplication m a = TaskApp
  { unTaskApp :: StateT HC.Tasks m a
  } deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, HC.HasTasks)

instance (Monad m, V.HasEventSink m) => V.HasEventSink (TaskApplication m) where
  writeEvent = lift . V.writeEvent

instance (Monad m, Provider k m) => Provider k (TaskApplication m) where
  provide = lift provide


runTask
  :: (MonadIO m, V.HasEventSource m)
  => TaskApplication m a -> m a
runTask op
  = V.readEvents
  >>= \vs -> evalStateT (replayEventLog vs >> unTaskApp op) HC.emptyTasks