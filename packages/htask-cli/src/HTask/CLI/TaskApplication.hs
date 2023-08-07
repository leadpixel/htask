{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HTask.CLI.TaskApplication
  ( CanRunTask
  , HasEventBackend
  , TaskApplication
  , runTask
  ) where

import qualified HTask.Core                as H
import qualified Leadpixel.Events          as V

import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Trans.State (StateT, evalStateT)
import           Data.Sequence             (Seq)
import           Leadpixel.Provider


type HasEventBackend m = (MonadIO m, V.HasEventSource m, V.HasEventSink m)
type CanRunTask m = (Monad m, HasEventBackend m)


newtype TaskApplication m a
  = TaskApp { unTaskApp :: StateT (Seq H.Task) m a }
  deriving (Applicative, Functor, H.HasTasks, Monad, MonadIO, MonadTrans)

instance (Monad m, V.HasEventSink m) => V.HasEventSink (TaskApplication m) where
  writeEvent = lift . V.writeEvent

instance (Monad m, Provider k m) => Provider k (TaskApplication m) where
  provide = lift provide


runTask
  :: (MonadIO m, V.HasEventSource m)
  => TaskApplication m a -> m a
runTask op
  = V.readEvents
  >>= \vs -> evalStateT (H.replayEventLog vs >> unTaskApp op) mempty
