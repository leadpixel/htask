{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module HTask.TaskApplication
  ( TaskApplication (..)
  , TaskConfig
  , runTask
  ) where

import Control.Monad.IO.Class
import Event
import HTask.Config
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as S
import qualified HTask                as H


type TaskConfig m = R.ReaderT GlobalOptions m


newtype TaskApplication m a = TaskApp
  { unwrapTaskApp :: S.StateT H.Tasks (TaskConfig m) a
  } deriving (Functor, Applicative, Monad)

instance H.HasTasks (TaskApplication IO) where
  getTasks = TaskApp H.getTasks
  addNewTask = TaskApp . H.addNewTask
  updateExistingTask ref = TaskApp . H.updateExistingTask ref
  removeTaskRef = TaskApp . H.removeTaskRef

instance (Monad m, CanTime m) => CanTime (TaskApplication m) where
  now = TaskApp $ S.lift $ R.lift now

instance (Monad m, CanUuid m) => CanUuid (TaskApplication m) where
  uuidGen = TaskApp $ S.lift $ R.lift uuidGen

instance HasEventSink (TaskApplication IO) where
  writeEvent ev
    = TaskApp $ S.lift (runWithFile $ writeEvent ev)


runWithFile :: (MonadIO m) => FileBackend m a -> TaskConfig m a
runWithFile (F x) = R.withReaderT taskfile x


prepTasks :: [H.TaskEvent] -> TaskConfig IO [H.Task]
prepTasks vs
  = S.execStateT
      (unwrapTaskApp $ H.replayEventLog vs)
      H.emptyTasks


runTask :: TaskApplication IO a -> TaskConfig IO a
runTask op
  = runWithFile readEvents
  >>= prepTasks
  >>= S.evalStateT (unwrapTaskApp op)
