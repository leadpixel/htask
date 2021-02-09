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

import qualified Control.Monad.Reader     as R
import qualified Control.Monad.State      as S
import qualified Control.Monad.Trans      as T
import qualified Data.Aeson               as A
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Foldable            as Foldable
import qualified Data.UUID                as UUID
import qualified Events                   as V
import qualified HTask.Core.TaskContainer as HC
import qualified HTask.Core.TaskEvent     as TV

import           Data.Sequence            (Seq (..))
import           Data.Time                (UTCTime)
import           Data.UUID                (UUID)
import           Event.Backend.Memory     (MemoryBackend, runMemoryBackend)
import           Leadpixel.Provider

import           Data.Maybe


newtype TaskAppT m a = TaskApp
  { unTaskApp :: S.StateT HC.Tasks m a
  } deriving (Functor, Applicative, Monad, T.MonadTrans)

instance (Monad m, V.HasEventSink m) => V.HasEventSink (TaskAppT m) where
  writeEvent = T.lift . V.writeEvent

instance (Monad m) => HC.HasTasks (TaskAppT m) where
  getTasks = TaskApp HC.getTasks
  addNewTask = TaskApp . HC.addNewTask
  updateExistingTask ref = TaskApp . HC.updateExistingTask ref
  removeTaskRef = TaskApp . HC.removeTaskRef

instance (Provider k m) => Provider k (TaskAppT m) where
  provide = T.lift provide


type Args = (UUID, UTCTime)

newtype DataProviderT m a = DataProvider
  { unDataProvider :: R.ReaderT Args m a
  } deriving (Functor, Applicative, Monad, T.MonadTrans)

instance (Monad m, V.HasEventSink m) => V.HasEventSink (DataProviderT m) where
  writeEvent = T.lift . V.writeEvent

instance (Monad m) => Provider UUID (DataProviderT m) where
  provide = fst <$> DataProvider R.ask

instance (Monad m) => Provider UTCTime (DataProviderT m) where
  provide = snd <$> DataProvider R.ask


newtype WriteFailureT m a = WriteFailure
  { unWriteFail :: S.StateT HC.Tasks m a
  } deriving (Functor, Applicative, Monad, T.MonadTrans)

instance (Monad m) => HC.HasTasks (WriteFailureT m) where
  getTasks = WriteFailure HC.getTasks
  addNewTask = WriteFailure . HC.addNewTask
  updateExistingTask ref = WriteFailure . HC.updateExistingTask ref
  removeTaskRef = WriteFailure . HC.removeTaskRef

-- instance (Monad m) => V.HasEventSink (WriteFailureT m) where
--   writeEvent = fail "called fail"


runStack :: (Monad m) => Args -> TaskAppT (DataProviderT (MemoryBackend m)) a -> m ((a, HC.Tasks), Seq BL.ByteString)
runStack args op
  = runMemoryBackend
  $ R.runReaderT
    ( unDataProvider
    $ S.runStateT
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
    extractLog :: Seq BL.ByteString -> [TV.TaskEvent]
    extractLog = mapMaybe A.decode . Foldable.toList


runWriteFailure :: (Monad m) => Args -> WriteFailureT m a -> m (a, HC.Tasks)
runWriteFailure _args op
  = S.runStateT
      (unWriteFail op)
      HC.emptyTasks
