{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Event.Backends
  ( HasEventSource (..)
  , HasEventSink (..)

  , FileBackend (..)
  -- , ConduitBackend (..)
  ) where

import Control.Exception
import qualified Control.Monad.Trans.Resource as Rt
import qualified Data.Aeson                   as A
import qualified Data.Conduit                 as C
import qualified Control.Monad.Reader         as R
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.UTF8    as UTF8
import qualified Data.Conduit.Combinators     as Cx
import qualified Data.Maybe as Mb

import Event.Event
import Data.Semigroup ((<>))
import Data.Conduit (($$), ($=))
import System.Exit


class HasEventSource  m where
  readEvents :: (A.FromJSON a) => m [Event a]

class HasEventSink m where
  writeEvent :: (A.ToJSON a) => Event a -> m ()




type FileBackend = R.ReaderT FilePath IO


instance HasEventSource FileBackend where
  readEvents = fileReadEvents

instance HasEventSink FileBackend where
  writeEvent = fileWriteEvent



handleReadError :: IOError -> Maybe ReadError
handleReadError e = Just (ReadError $ show e)


handleWriteError :: IOError -> Maybe ReadError
handleWriteError _ = Just (ReadError "fuck")



newtype ReadError = ReadError String
  deriving (Show)


fileReadEvents :: (A.FromJSON a) => FileBackend [Event a]
fileReadEvents = do
  file <- R.ask
  R.liftIO $ print "before read"
  xs <- R.liftIO (readCatch file)
  R.liftIO $ print "after read"

  case xs of
    Left e -> do
      R.liftIO $ print "left"
      R.liftIO $ exitFailure

    Right v ->
      pure (decodeEvents v)

  where
    decodeEvents :: (A.FromJSON b) => String -> [Event b]
    decodeEvents = Mb.catMaybes . fmap (A.decode . UTF8.fromString) . lines

    readCatch :: FilePath -> IO (Either ReadError String)
    readCatch f = tryJust handleReadError (readFile f)


fileWriteEvent :: (A.ToJSON a) => Event a -> FileBackend ()
fileWriteEvent ev = do
  let xs = bsEncode ev

  file <- R.ask
  R.liftIO $ print "before append"
  R.liftIO (appendCatch file xs)
  R.liftIO $ print "after append"

  where
    bsEncode :: (A.ToJSON b) => b -> BS.ByteString
    bsEncode
      = BL.toStrict
      . flip mappend "\n"
      . A.encode

    appendCatch :: FilePath -> BS.ByteString -> IO (Either ReadError ())
    appendCatch f x = tryJust handleWriteError (BS.appendFile f x)




-- type ConduitBackend = R.ReaderT FilePath IO


-- instance HasEventSource ConduitBackend where
--   readEvents = conduitReadEvents
--
-- instance HasEventSink ConduitBackend where
--   writeEvent = conduitWriteEvent



-- conduitReadEvents
--   :: (A.FromJSON a)
--   => R.ReaderT FilePath IO [Event a]
-- conduitReadEvents
--   = R.ask >>= \file
--   -> R.lift
--     $ t <$> Rt.runResourceT
--       (  Cx.sourceFile file
--       $= splitLines
--       $$ Cx.sinkList
--       )

--   where
--     t :: (A.FromJSON a) => [BS.ByteString] -> [Event a]
--     t = Mb.catMaybes . fmap A.decodeStrict


-- conduitWriteEvent
--   :: (A.ToJSON a)
--   => Event a
--   -> R.ReaderT FilePath IO ()
-- conduitWriteEvent x
--   = R.ask >>= \file
--   -> R.lift
--     $ Rt.runResourceT
--       $ C.yield (convert x)
--       $$ Cx.sinkFile file

--   where
--     convert :: (A.ToJSON a) => Event a -> BS.ByteString
--     convert e = BL.toStrict (A.encode e) <> "\n"



-- splitLines :: C.Conduit BS.ByteString (Rt.ResourceT IO) BS.ByteString
-- splitLines = Cx.linesUnboundedAscii
