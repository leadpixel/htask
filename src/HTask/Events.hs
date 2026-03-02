{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module HTask.Events
  ( Event (..)
  , HasEventSink (..)
  , HasEventSource (..)
  , createEvent
    -- * File Backend
  , FileEventBackend ()
  , runFileBackend
    -- * Memory Backend
  , MemoryEventBackend ()
  , runBackend -- For tests
  , runMemoryBackend
  , runMemoryBackend'
  ) where

import qualified Control.Monad.Trans.State  as State
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as Strict
import qualified Data.ByteString.Lazy       as Lazy
import qualified Data.Either
import qualified Data.Sequence              as Seq
import qualified System.IO                  as Sys

import           Conduit                    (ConduitT, runConduit, (.|))
import qualified Conduit
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.IO.Unlift    (MonadUnliftIO, withRunInIO)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Control.Monad.Trans.State  (StateT, runStateT)
import           Data.Foldable              (foldl', toList)
import           Data.Sequence              (Seq (..), (|>))
import           Data.Time                  (UTCTime)
import           GHC.Generics               (Generic)
import           HTask.Provider


-- | Core Event Types
class (Monad m) => HasEventSource m where
  readEvents :: (Aeson.FromJSON a) => m [Event a]

class (Monad m) => HasEventSink m where
  writeEvent :: (Aeson.ToJSON a) => Event a -> m ()
  writeEvents :: (Aeson.ToJSON a) => [Event a] -> m ()
  writeEvents = mapM_ writeEvent

data Event a
  = Event
    { timestamp :: UTCTime
    , payload   :: a
    }
  deriving (Eq, Generic, Show)

instance (Aeson.ToJSON a) => Aeson.ToJSON (Event a)
instance (Aeson.FromJSON a) => Aeson.FromJSON (Event a)

createEvent :: (Provider UTCTime m) => a -> m (Event a)
createEvent x = do
  t <- provide @UTCTime
  pure $ Event { timestamp = t , payload = x }


-- | File Backend
newtype FileEventBackend m a
  = FileBackend { unFileBackend :: ReaderT FilePath m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

deriving via (ReaderT FilePath m)
  instance (MonadUnliftIO m) => MonadUnliftIO (FileEventBackend m)

instance (MonadUnliftIO m) => HasEventSource (FileEventBackend m) where
  readEvents = FileBackend (ReaderT (\path -> do
    ks <- decodeEvents . zip [1..] <$> loadFileLines Conduit.sinkList path
    reportErrors ks
    pure $ Data.Either.rights ks))
    where
      reportErrors :: (MonadIO m) => [Either (Int, Strict.ByteString) a] -> m ()
      reportErrors = mapM_ reportError . Data.Either.lefts

      reportError :: (MonadIO m) => (Int, Strict.ByteString) -> m ()
      reportError (n, bs) = liftIO $ Sys.hPutStrLn Sys.stderr $
        "Warning: Failed to decode event on line " <> show n <> ": " <> show bs

      loadFileLines c file = Conduit.withSourceFile file $ \src ->
        runConduit $ src .| Conduit.linesUnboundedAsciiC .| c

instance (MonadUnliftIO m) => HasEventSink (FileEventBackend m) where
  writeEvent e = FileBackend $ ReaderT $ \path -> fileAppend [encodeEvent e] path
  writeEvents es = FileBackend $ ReaderT $ \path -> fileAppend (encodeEvent <$> es) path

runFileBackend :: FilePath -> FileEventBackend m a -> m a
runFileBackend file op = runReaderT (unFileBackend op) file

fileAppend :: (Monad m, MonadUnliftIO m) => [Strict.ByteString] -> FilePath -> m ()
fileAppend xs file = withAppendSinkFile file $ \dest ->
  runConduit $ Conduit.yieldMany xs .| dest

decodeEvents :: (Aeson.FromJSON a) => [(Int, Strict.ByteString)] -> [Either (Int, Strict.ByteString) a]
decodeEvents = fmap (\(n, x) -> maybe (Left (n, x)) Right (Aeson.decodeStrict x))

encodeEvent :: (Aeson.ToJSON a) => Event a -> Strict.ByteString
encodeEvent e = Lazy.toStrict (Aeson.encode e) <> "\n"

withAppendSinkFile :: (MonadUnliftIO m, MonadIO n) => FilePath -> (ConduitT Strict.ByteString o n () -> m a) -> m a
withAppendSinkFile path op = withRunInIO $ \run ->
  Sys.withBinaryFile path Sys.AppendMode $ run . op . Conduit.sinkHandle


-- | Memory Backend
newtype MemoryEventBackend m a
  = MemoryBackend { runBackend :: StateT (Seq Lazy.ByteString) m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO, MonadTrans)

instance (Monad m) => HasEventSource (MemoryEventBackend m) where
  readEvents = toList <$> MemoryBackend (State.gets decodeSequence)
    where
      decodeSequence :: (Aeson.FromJSON a) => Seq Lazy.ByteString -> Seq (Event a)
      decodeSequence = foldl' maybeAccumulate Seq.empty . fmap Aeson.decode
      maybeAccumulate acc = maybe acc (acc |>)

instance (Monad m) => HasEventSink (MemoryEventBackend m) where
  writeEvent ev = MemoryBackend $ State.modify (\xs -> xs |> Aeson.encode ev)

runMemoryBackend :: (Monad m) => MemoryEventBackend m a -> m a
runMemoryBackend op = fst <$> runStateT (runBackend op) mempty

runMemoryBackend' :: (Monad m) => MemoryEventBackend m a -> m (a, Seq Lazy.ByteString)
runMemoryBackend' op = runStateT (runBackend op) mempty
