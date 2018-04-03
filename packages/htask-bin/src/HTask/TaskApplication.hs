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

import Event
import HTask.Config
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as S
import qualified HTask                as H


type TaskConfig = R.ReaderT GlobalOptions IO


runWithFile :: FileBackend a -> TaskConfig a
runWithFile (F x) = R.withReaderT taskfile x


newtype TaskApplication a = TaskApp
  { unwrapTaskApp :: S.StateT H.Tasks TaskConfig a
  } deriving (Functor, Applicative, Monad)


instance H.HasTasks TaskApplication where
  getTasks = TaskApp H.getTasks
  addNewTask = TaskApp . H.addNewTask
  updateExistingTask ref = TaskApp . H.updateExistingTask ref
  removeTaskRef = TaskApp . H.removeTaskRef


instance CanTime TaskApplication where
  now = TaskApp $ S.lift $ R.lift now


instance CanUuid TaskApplication where
  uuidGen = TaskApp $ S.lift $ R.lift uuidGen


-- instance HasEventSink TaskApplication where
--   writeEvent ev
--     = TaskApp $ S.lift (runWithFile $ writeEvent ev)


-- runWithFile :: ConduitBackend a -> E.ExceptT String TaskConfig a
-- runWithFile k
--   = do
--     c <- ask
--     let c' = taskfile c

--     let k' = R.runReaderT c'

instance HasEventSink TaskApplication where
  writeEvent ev
    = TaskApp $ S.lift (runWithFile $ writeEvent ev)


prepTasks :: [H.TaskEvent] -> TaskConfig [H.Task]
prepTasks vs
  = S.execStateT
      (unwrapTaskApp $ H.replayEventLog vs)
      H.emptyTasks


runTask :: TaskApplication a -> TaskConfig a
runTask op
  = runWithFile readEvents
  >>= prepTasks
  >>= S.evalStateT (unwrapTaskApp op)
