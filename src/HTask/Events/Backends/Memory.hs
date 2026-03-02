{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HTask.Events.Backends.Memory
  ( MemoryEventBackend ()
  , runBackend -- Export constructor field for test setup
  , runMemoryBackend
  , runMemoryBackend'
  ) where

import qualified Control.Monad.Trans.State as State
import qualified Data.Aeson                as Aeson
import qualified Data.ByteString.Lazy      as Lazy
import qualified Data.Sequence             as Seq
import qualified HTask.Events              as V

import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.Monad.Trans.State (StateT, runStateT)
import           Data.Foldable             (foldl', toList)
import           Data.Sequence             (Seq (..), (|>))


newtype MemoryEventBackend m a
  = Backend { runBackend :: StateT (Seq Lazy.ByteString) m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO, MonadTrans)

instance (Monad m) => V.HasEventSource (MemoryEventBackend m) where
  readEvents = toList <$> memoryReadEvents

instance (Monad m) => V.HasEventSink (MemoryEventBackend m) where
  writeEvent = memoryWriteEvent


runMemoryBackend :: (Monad m) => MemoryEventBackend m a -> m a
runMemoryBackend op = fst <$> runStateT (runBackend op) mempty

runMemoryBackend' :: (Monad m) => MemoryEventBackend m a -> m (a, Seq Lazy.ByteString)
runMemoryBackend' op = runStateT (runBackend op) mempty


memoryReadEvents :: (Monad m, Aeson.FromJSON a) => MemoryEventBackend m (Seq (V.Event a))
memoryReadEvents = Backend (State.gets decodeEvents)
  where
    decodeEvents :: (Aeson.FromJSON a) => Seq Lazy.ByteString -> Seq (V.Event a)
    decodeEvents = foldl' maybeAccumulate Seq.empty . fmap Aeson.decode

    maybeAccumulate :: Seq a -> Maybe a -> Seq a
    maybeAccumulate acc = maybe acc (acc |>)


memoryWriteEvent :: (Monad m, Aeson.ToJSON a) => V.Event a -> MemoryEventBackend m ()
memoryWriteEvent ev = Backend $ State.modify (\xs -> xs |> Aeson.encode ev)
