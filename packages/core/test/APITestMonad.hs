{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module APITestMonad
  ( TaskAppT (..)
  , DataProviderT (..)
  , runApi
  , runTasks
  , runEventLog
  , runWriteFailure
  ) where

import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State  as State
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as Lazy
import qualified Data.Foldable              as Foldable
import qualified Events                     as V
import qualified HTask.Core.TaskContainer   as HC
import qualified HTask.Core.TaskEvent       as TV

import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Trans.State  (StateT, runStateT)
import           Data.Sequence              (Seq (..))
import           Data.Time                  (UTCTime)
import           Data.UUID                  (UUID)
import           Event.Backend.Memory       (MemoryBackend, runMemoryBackend)
import           Leadpixel.Provider

import           Data.Maybe


newtype TaskAppT m a = TaskApp
  { unTaskApp :: StateT HC.Tasks m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

instance (Monad m, V.HasEventSink m) => V.HasEventSink (TaskAppT m) where
  writeEvent = lift . V.writeEvent

instance (Monad m) => HC.HasTasks (TaskAppT m) where
  getTasks = TaskApp HC.getTasks
  addNewTask = TaskApp . HC.addNewTask
  updateExistingTask ref = TaskApp . HC.updateExistingTask ref
  removeTaskRef = TaskApp . HC.removeTaskRef

instance (Monad m, Provider k m) => Provider k (TaskAppT m) where
  provide = lift provide


type Args = (UUID, UTCTime)

newtype DataProviderT m a = DataProvider
  { unDataProvider :: ReaderT Args m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

instance (Monad m, V.HasEventSink m) => V.HasEventSink (DataProviderT m) where
  writeEvent = lift . V.writeEvent

instance (Monad m) => Provider UUID (DataProviderT m) where
  provide = fst <$> DataProvider Reader.ask

instance (Monad m) => Provider UTCTime (DataProviderT m) where
  provide = snd <$> DataProvider Reader.ask


newtype WriteFailureT m a = WriteFailure
  { unWriteFail :: StateT HC.Tasks m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

instance (Monad m) => HC.HasTasks (WriteFailureT m) where
  getTasks = WriteFailure HC.getTasks
  addNewTask = WriteFailure . HC.addNewTask
  updateExistingTask ref = WriteFailure . HC.updateExistingTask ref
  removeTaskRef = WriteFailure . HC.removeTaskRef

-- instance (Monad m) => V.HasEventSink (WriteFailureT m) where
--   writeEvent = fail "called fail"


runStack :: (Monad m) => Args -> TaskAppT (DataProviderT (MemoryBackend m)) a -> m ((a, HC.Tasks), Seq Lazy.ByteString)
runStack args op
  = runMemoryBackend
  $ runReaderT
    ( unDataProvider
    $ runStateT
      (unTaskApp op)
      HC.emptyTasks
    )
    args


runApi :: (Monad m) => Args -> TaskAppT (DataProviderT (MemoryBackend m)) a -> m a
runApi args op
  = fst . fst <$> runStack args op


runTasks :: (Monad m) => Args -> TaskAppT (DataProviderT (MemoryBackend m)) a -> m HC.Tasks
runTasks args op
  = snd . fst <$> runStack args op


runEventLog :: (Monad m) => Args -> TaskAppT (DataProviderT (MemoryBackend m)) a -> m [TV.TaskEvent]
runEventLog args op
  = extractLog . snd <$> runStack args op

  where
    extractLog :: Seq Lazy.ByteString -> [TV.TaskEvent]
    extractLog = mapMaybe Aeson.decode . Foldable.toList


runWriteFailure :: (Monad m) => Args -> WriteFailureT m a -> m (a, HC.Tasks)
runWriteFailure _args op
  = runStateT
      (unWriteFail op)
      HC.emptyTasks
