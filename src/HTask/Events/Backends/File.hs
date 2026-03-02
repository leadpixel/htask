{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module HTask.Events.Backends.File
  ( FileEventBackend ()
  , runFileBackend
  ) where

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as Strict
import qualified Data.ByteString.Lazy       as Lazy
import qualified System.IO                  as Sys

import           Conduit                    (ConduitT, Void, runConduit, (.|))
import qualified Conduit
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.IO.Unlift    (MonadUnliftIO, withRunInIO)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.Either
import           HTask.Events


newtype FileEventBackend m a
  = Backend { unBackend :: ReaderT FilePath m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans)

deriving via (ReaderT FilePath m)
  instance (MonadUnliftIO m) => MonadUnliftIO (FileEventBackend m)

instance (MonadUnliftIO m) => HasEventSource (FileEventBackend m) where
  readEvents = conduitReadEvents


instance (MonadUnliftIO m) => HasEventSink (FileEventBackend m) where
  writeEvent e  = Backend $ ReaderT $ \path -> fileAppend [encodeEvent e] path
  writeEvents es = Backend $ ReaderT $ \path -> fileAppend (encodeEvent <$> es) path


runFileBackend :: FilePath -> FileEventBackend m a -> m a
runFileBackend file op
  = runReaderT (unBackend op) file


conduitReadEvents :: (MonadUnliftIO m, Aeson.FromJSON a) => FileEventBackend m [a]
conduitReadEvents
  = Backend (ReaderT (\path -> do
      ks <- decodeEvents . zip [1..] <$> loadFileLines Conduit.sinkList path
      reportErrors ks
      pure $ Data.Either.rights ks
                     ))

  where
    reportErrors :: (MonadIO m) => [Either (Int, Strict.ByteString) a] -> m ()
    reportErrors = mapM_ reportError . Data.Either.lefts

    reportError :: (MonadIO m) => (Int, Strict.ByteString) -> m ()
    reportError (n, bs) = liftIO $
      Sys.hPutStrLn Sys.stderr $
        "Warning: Failed to decode event on line " <> show n <> ": " <> show bs

    loadFileLines :: (Monad m, MonadUnliftIO m) => ConduitT Strict.ByteString Conduit.Void m [Strict.ByteString] -> FilePath -> m [Strict.ByteString]
    loadFileLines c file
      = Conduit.withSourceFile file $ \src ->
          runConduit $ src .| Conduit.linesUnboundedAsciiC .| c


fileAppend :: (Monad m, MonadUnliftIO m) => [Strict.ByteString] -> FilePath -> m ()
fileAppend xs file =
  withAppendSinkFile file $ \dest ->
    runConduit $ Conduit.yieldMany xs .| dest


decodeEvents :: (Aeson.FromJSON a) => [(Int, Strict.ByteString)] -> [Either (Int, Strict.ByteString) a]
decodeEvents = fmap (\(n, x) -> maybe (Left (n, x)) Right (Aeson.decodeStrict x))


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
