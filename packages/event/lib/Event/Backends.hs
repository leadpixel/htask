{-# LANGUAGE OverloadedStrings          #-}

module Event.Backends
  ( HasEventSource (..)
  , HasEventSink (..)

  , FileBackend (..)
  ) where

import Control.Exception
import qualified Control.Monad.Trans.Resource as Rt
import qualified Data.Aeson                   as A
import qualified Data.Conduit                 as C
import qualified Control.Monad.Reader         as R
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit.Combinators     as Cx
import qualified Data.Maybe as Mb

import Event.Event
import Data.Semigroup ((<>))
import Data.Conduit (($$), ($=))
import System.Exit
import System.IO.Error
import qualified System.IO                   as IO
import GHC.IO.Exception


class HasEventSource  m where
  readEvents :: (A.FromJSON a) => m [Event a]

class HasEventSink m where
  writeEvent :: (A.ToJSON a) => Event a -> m ()


newtype FileError = FileError String
  deriving (Show)


handleFileError :: IOException -> Maybe FileError
handleFileError e
  = Just $ case ioeGetErrorType e of
      AlreadyExists     -> FileError "AlreadyExists"
      NoSuchThing       -> FileError "NoSuchThing"
      ResourceBusy      -> FileError "ResourceBusy"
      ResourceExhausted -> FileError "ResourceExhausted"
      EOF               -> FileError "EOF"
      IllegalOperation  -> FileError "IllegalOperation"
      PermissionDenied  -> FileError "PermissionDenied"
      UserError         -> FileError "UserError"


newtype FileBackend a = F (R.ReaderT FilePath IO a)


instance HasEventSource FileBackend where
  readEvents = conduitReadEvents

instance HasEventSink FileBackend where
  writeEvent = conduitWriteEvent


conduitReadEvents :: (A.FromJSON a) => FileBackend [Event a]
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


conduitWriteEvent :: (A.ToJSON a) => Event a -> FileBackend ()
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


orDie :: (a -> b) -> Either FileError a -> IO b
orDie _ (Left e) = die (show e)
orDie f (Right v) = pure (f v)
