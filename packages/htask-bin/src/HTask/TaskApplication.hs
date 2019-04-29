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
import           Event
import qualified HTask               as H


type HasEventBackend m = (Monad m, HasEventSource m, HasEventSink m)


newtype TaskApplication m a = TaskApp
  { runTaskApp :: S.StateT H.Tasks m a
  } deriving (Functor, Applicative, Monad, T.MonadTrans)

instance (Monad m) => H.HasTasks (TaskApplication m) where
  getTasks = TaskApp H.getTasks
  addNewTask = TaskApp . H.addNewTask
  updateExistingTask ref = TaskApp . H.updateExistingTask ref
  removeTaskRef = TaskApp . H.removeTaskRef

instance (Monad m, CanTime m) => CanTime (TaskApplication m) where
  now = T.lift now

instance (Monad m, CanUuid m) => CanUuid (TaskApplication m) where
  uuidGen = T.lift uuidGen

instance (Monad m, HasEventSink m) => HasEventSink (TaskApplication m) where
  writeEvent ev
    = TaskApp $ T.lift (writeEvent ev)


runTask
  :: (Monad m, HasEventSource m)
  => TaskApplication m a -> m a
runTask op
  = readEvents
  >>= \vs -> S.evalStateT (H.replayEventLog vs >> runTaskApp op) H.emptyTasks
