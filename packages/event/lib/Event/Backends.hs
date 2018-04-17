{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Event.Backends
  ( HasEventSource (..)
  , HasEventSink (..)

  , FileBackend (runFileBackend)
  ) where

import qualified Control.Monad.Reader         as R
import qualified Control.Monad.Trans          as T
import qualified Control.Monad.Trans.Resource as Rt
import qualified Data.Aeson                   as A
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Combinators     as Cx
import qualified Data.Maybe                   as Mb
import qualified System.IO                    as IO

import Control.Exception
import Control.Monad.IO.Class
import Data.Conduit (($$), ($=))
import Data.Semigroup ((<>))
import Event.Event
import GHC.IO.Exception
import System.Exit
import System.IO.Error


class HasEventSource m where
  readEvents :: (A.FromJSON a) => m [Event a]

class HasEventSink m where
  writeEvent :: (A.ToJSON a) => Event a -> m ()


newtype FileError = FileError String
  deriving (Show)


newtype FileBackend m a = F
  { runFileBackend :: R.ReaderT FilePath m a
  } deriving (Functor, Applicative, Monad)

instance T.MonadTrans FileBackend where
  lift = F . T.lift

instance (MonadIO m) => HasEventSource (FileBackend m) where
  readEvents = conduitReadEvents

instance (MonadIO m) => HasEventSink (FileBackend m) where
  writeEvent = conduitWriteEvent


conduitReadEvents :: (MonadIO m, A.FromJSON a) => FileBackend m [Event a]
conduitReadEvents
  = F $ R.ask >>= \file
  -> R.liftIO $ handleReadFile file >>= orDie decodeEvents

  where
    handleReadFile :: FilePath -> IO (Either FileError [BS.ByteString])
    handleReadFile = runFileOp loadFileLines

    loadFileLines :: FilePath -> IO [BS.ByteString]
    loadFileLines file
      = Rt.runResourceT
        $ Cx.sourceFile file
        $= splitLines
        $$ Cx.sinkList

    decodeEvents :: (A.FromJSON a) => [BS.ByteString] -> [Event a]
    decodeEvents = Mb.catMaybes . fmap A.decodeStrict


conduitWriteEvent :: (MonadIO m, A.ToJSON a) => Event a -> FileBackend m ()
conduitWriteEvent x
  = F $ R.ask >>= \file
  -> R.liftIO $ handleWriteFile file >>= orDie (const ())

  where
    handleWriteFile :: FilePath -> IO (Either FileError ())
    handleWriteFile = runFileOp (fileAppend $ encodeEvent x)

    fileAppend :: BS.ByteString -> FilePath -> IO ()
    fileAppend x' file
      = Rt.runResourceT
        $ C.yield x'
        $$ sinkFileAppend file

    encodeEvent :: (A.ToJSON a) => Event a -> BS.ByteString
    encodeEvent e = BL.toStrict (A.encode e) <> "\n"

    sinkFileAppend fp
      = Cx.sinkIOHandle (IO.openBinaryFile fp IO.AppendMode)


splitLines :: C.Conduit BS.ByteString (Rt.ResourceT IO) BS.ByteString
splitLines = Cx.linesUnboundedAscii


runFileOp :: (FilePath -> IO a) -> FilePath -> IO (Either FileError a)
runFileOp t = tryJust handleFileError . t


handleFileError :: IOException -> Maybe FileError
handleFileError e
  = Just $ case ioeGetErrorType e of
      NoSuchThing      -> FileError "task file does not exist"
      PermissionDenied -> FileError "unable to read file"
      e'               -> FileError $ show e'


orDie :: (a -> b) -> Either FileError a -> IO b
orDie _ (Left e) = die (show e)
orDie f (Right v) = pure (f v)
