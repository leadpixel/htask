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


class HasEventSource  m where
  readEvents :: (A.FromJSON a) => m [Event a]

class HasEventSink m where
  writeEvent :: (A.ToJSON a) => Event a -> m ()




type FileBackend = R.ReaderT FilePath IO


instance HasEventSource FileBackend where
  readEvents = fileReadEvents

instance HasEventSink FileBackend where
  writeEvent = fileWriteEvent


fileReadEvents :: (A.FromJSON a) => FileBackend [Event a]
fileReadEvents = do
  q <- R.ask
  xs <- R.liftIO (kkk q)

  case xs of
    Left e -> undefined
    Right v -> pure (y v)

  where
    y :: (A.FromJSON b) => String -> [Event b]
    y = Mb.catMaybes . fmap (A.decode . UTF8.fromString) . lines

    kkk :: FilePath -> IO (Either ReadError String)
    kkk f = tryJust j (readFile f)

    j :: PermissionDenied -> Maybe ReadError
    j = undefined


newtype ReadError = ReadError String


fileWriteEvent :: (A.ToJSON a) => Event a -> FileBackend ()
fileWriteEvent ev = R.ask >>= \z -> R.liftIO (t z ev)
  where
    t :: (A.ToJSON b) => FilePath -> b -> IO ()
    t f = BS.appendFile f
        . BL.toStrict
        . flip mappend "\n"
        . A.encode




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
