{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Leadpixel.Events.Backends.File
  ( FileEventBackend ()
  , runFileBackend
  ) where

import qualified Conduit
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as Strict
import qualified Data.ByteString.Lazy       as Lazy
import qualified System.IO                  as Sys

import           Conduit                    (ConduitT, Void, runConduit, (.|))
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.IO.Unlift    (MonadUnliftIO, withRunInIO)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.Maybe                 (mapMaybe)
import           Leadpixel.Events


newtype FileEventBackend m a
  = Backend { runBackend :: ReaderT FilePath m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance (MonadUnliftIO m) => HasEventSource (FileEventBackend m) where
  readEvents = conduitReadEvents
  readEventsStream = conduitReadStream

instance (MonadUnliftIO m) => HasEventSink (FileEventBackend m) where
  writeEvent = conduitWriteEvent
  writeEvents = conduitWriteMany
  writeEventsStream = conduitWriteStream


runFileBackend :: FilePath -> FileEventBackend m a -> m a
runFileBackend = flip (runReaderT . runBackend)


conduitReadStream :: ConduitT () (Event a) (FileEventBackend m) ()
conduitReadStream = undefined


conduitWriteStream :: ConduitT (Event a) Void (FileEventBackend m) ()
conduitWriteStream = undefined


conduitReadEvents :: (MonadUnliftIO m, Aeson.FromJSON a) => FileEventBackend m [a]
conduitReadEvents
  = Backend (ReaderT (fmap decodeEvents . loadFileLines Conduit.sinkList))

  where
    loadFileLines :: (Monad m, MonadUnliftIO m) => ConduitT Strict.ByteString Conduit.Void m [Strict.ByteString] -> FilePath -> m [Strict.ByteString]
    loadFileLines c file
      = Conduit.withSourceFile file $ \src ->
          runConduit $ src .| Conduit.linesUnboundedAsciiC .| c


conduitWriteEvent :: (MonadUnliftIO m, Aeson.ToJSON a) => Event a -> FileEventBackend m ()
conduitWriteEvent =
  Backend . ReaderT . fileAppend . encodeEvent

  where
    fileAppend :: (Monad m, MonadUnliftIO m) => Strict.ByteString -> FilePath -> m ()
    fileAppend x file =
      withAppendSinkFile file $ \dest ->
        runConduit $ Conduit.yield x .| dest


conduitWriteMany :: (MonadUnliftIO m, Aeson.ToJSON a) => [Event a] -> FileEventBackend m ()
conduitWriteMany =
  Backend . ReaderT . fileAppend . fmap encodeEvent

  where
    fileAppend :: (Monad m, MonadUnliftIO m) => [Strict.ByteString] -> FilePath -> m ()
    fileAppend xs file =
      Conduit.withSinkFile file $ \dest ->
        runConduit $ Conduit.yieldMany xs .| dest


decodeEvents :: (Aeson.FromJSON a) => [Strict.ByteString] -> [a]
decodeEvents = mapMaybe Aeson.decodeStrict


encodeEvent :: (Aeson.ToJSON a) => Event a -> Strict.ByteString
encodeEvent e = Lazy.toStrict (Aeson.encode e) <> "\n"


withAppendSinkFile
  :: (MonadUnliftIO m, MonadIO n)
  => FilePath
  -> (ConduitT Strict.ByteString o n () -> m a)
  -> m a
withAppendSinkFile path op =
  withRunInIO $ \run ->
    Sys.withBinaryFile path Sys.AppendMode $
      run . op . Conduit.sinkHandle
