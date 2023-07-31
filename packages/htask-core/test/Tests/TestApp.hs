{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Tests.TestApp
  ( TaskAppT (..)
  , DataProviderT (..)
  , runApi
  , runTasks
  , runEventLog
  , runWriteFailure
  ) where

import qualified Control.Monad.Trans.Reader      as Reader
import qualified Control.Monad.Trans.State       as State
import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString.Lazy            as Lazy
import qualified Data.Foldable                   as Foldable
import qualified HTask.Core                      as H
import qualified Leadpixel.Events                as V

import           Control.Monad.IO.Class          (MonadIO)
import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           Control.Monad.Trans.Reader      (ReaderT, runReaderT)
import           Control.Monad.Trans.State       (StateT, runStateT)
import           Data.Sequence                   (Seq)
import           Data.Time                       (UTCTime)
import           Data.UUID                       (UUID)
import           Leadpixel.Events.Backend.Memory (MemoryBackend,
                                                  runMemoryBackend)
import           Leadpixel.Provider

import           Data.Maybe


newtype TaskAppT m a = TaskApp
  { unTaskApp :: StateT (Seq H.Task) m a
  } deriving (Functor, Applicative, Monad, MonadTrans, H.HasTasks, MonadIO)

instance (Monad m, V.HasEventSink m) => V.HasEventSink (TaskAppT m) where
  writeEvent = lift . V.writeEvent

instance (Monad m, Provider k m) => Provider k (TaskAppT m) where
  provide = lift provide


type Args = (UUID, UTCTime)

-- TODO: what is the point of this?
newtype DataProviderT m a = DataProvider
  { unDataProvider :: ReaderT Args m a
  } deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (Monad m, V.HasEventSink m) => V.HasEventSink (DataProviderT m) where
  writeEvent = lift . V.writeEvent

instance (Monad m) => Provider UUID (DataProviderT m) where
  provide = fst <$> DataProvider Reader.ask

instance (Monad m) => Provider UTCTime (DataProviderT m) where
  provide = snd <$> DataProvider Reader.ask


newtype WriteFailureT m a = WriteFailure
  { unWriteFail :: StateT (Seq H.Task) m a
  } deriving (Functor, Applicative, Monad, MonadTrans, H.HasTasks)


runStack :: (Monad m) => Args -> TaskAppT (DataProviderT (MemoryBackend m)) a -> m ((a, Seq H.Task), Seq Lazy.ByteString)
runStack args op
  = runMemoryBackend
  $ runReaderT
    ( unDataProvider
    $ runStateT
      (unTaskApp op)
      mempty
    )
    args


runApi :: (Monad m) => Args -> TaskAppT (DataProviderT (MemoryBackend m)) a -> m a
runApi args op
  = fst . fst <$> runStack args op


runTasks :: (Monad m) => Args -> TaskAppT (DataProviderT (MemoryBackend m)) a -> m (Seq H.Task)
runTasks args op
  = snd . fst <$> runStack args op


runEventLog :: (Monad m) => Args -> TaskAppT (DataProviderT (MemoryBackend m)) a -> m [H.TaskEvent]
runEventLog args op
  = extractLog . snd <$> runStack args op

  where
    extractLog :: Seq Lazy.ByteString -> [H.TaskEvent]
    extractLog = mapMaybe Aeson.decode . Foldable.toList


runWriteFailure :: (Monad m) => Args -> WriteFailureT m a -> m (a, Seq H.Task)
runWriteFailure _args op
  = runStateT
      (unWriteFail op)
      mempty
