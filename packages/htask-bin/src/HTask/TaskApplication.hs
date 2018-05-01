{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module HTask.TaskApplication
  ( TaskApplication
  , HasEventBackend
  , runTask
  ) where

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as S
import qualified Control.Monad.Trans  as T
import qualified HTask                as H
import Event


type HasEventBackend m = (Monad m, HasEventSource m, HasEventSink m)


newtype TaskApplication m a = TaskApp
  { runTaskApp :: S.StateT H.Tasks m a
  } deriving (Functor, Applicative, Monad)

instance T.MonadTrans TaskApplication where
  lift = TaskApp . T.lift

instance (Monad m) => H.HasTasks (TaskApplication m) where
  getTasks = TaskApp H.getTasks
  addNewTask = TaskApp . H.addNewTask
  updateExistingTask ref = TaskApp . H.updateExistingTask ref
  removeTaskRef = TaskApp . H.removeTaskRef

instance (Monad m, CanTime m) => CanTime (TaskApplication m) where
  now = T.lift now

-- instance (Monad m, T.MonadTrans t, CanRandom m) => CanRandom (t m) where
--   getRandomRange = T.lift . getRandomRange

instance (Monad m, CanUuid m) => CanUuid (TaskApplication m) where
  uuidGen = T.lift uuidGen

-- instance (Monad m, CanUuid m) => CanUuid (TaskApplication m) where
--   uuidGen = TaskApp $ T.lift uuidGen

-- instance (Monad m, HasEventSource m) => HasEventSource (TaskApplication m) where
--   readEvents
--     = TaskApp $ T.lift readEvents

instance (Monad m, HasEventSink m) => HasEventSink (TaskApplication m) where
  writeEvent ev
    = TaskApp $ T.lift (writeEvent ev)


runTask
  :: (Monad m, HasEventSource m, HasEventSink m)
  => TaskApplication m a -> m a
runTask op
  = readEvents
  >>= \vs -> S.evalStateT (H.replayEventLog vs >> runTaskApp op) H.emptyTasks
