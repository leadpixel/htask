{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module HTask.TaskApplication
  ( TaskApplication (..)
  , EventBackend
  , runTask
  ) where

import Control.Monad.IO.Class
import Event
import HTask.Config
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as S
import qualified Control.Monad.Trans  as T
import qualified HTask                as H


type TaskLayer m a = S.StateT H.Tasks m a

type EventBackend m = FileBackend m

newtype TaskApplication m a = TaskApp
  { unwrapTaskApp :: TaskLayer (FileBackend m) a
  } deriving (Functor, Applicative, Monad)

instance T.MonadTrans TaskApplication where
  lift = TaskApp . T.lift . T.lift

instance H.HasTasks (TaskApplication IO) where
  getTasks = TaskApp H.getTasks
  addNewTask = TaskApp . H.addNewTask
  updateExistingTask ref = TaskApp . H.updateExistingTask ref
  removeTaskRef = TaskApp . H.removeTaskRef

instance (Monad m, CanTime m) => CanTime (TaskApplication m) where
  now = TaskApp $ S.lift $ R.lift now

instance (Monad m, CanUuid m) => CanUuid (TaskApplication m) where
  uuidGen = TaskApp $ S.lift $ R.lift uuidGen

instance (MonadIO m) => HasEventSink (TaskApplication m) where
  writeEvent ev
    = TaskApp $ S.lift (writeEvent ev)


runTask
  :: (H.HasTasks (TaskApplication m), MonadIO m)
  => TaskApplication m a -> FileBackend m a
runTask op
  = readEvents
  >>= \vs -> S.evalStateT (unwrapTaskApp $ H.replayEventLog vs >> op) H.emptyTasks
