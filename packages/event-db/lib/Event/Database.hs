{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Event.Database
  ( SQLEventBackend
  , prepareDB
  , runSql
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text.Encoding
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import qualified Control.Monad.Reader         as R
import qualified Data.Aeson                   as A
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Text                    as Text
import qualified Event                        as V


share [mkMigrate "migrateAll", mkPersist sqlSettings] [persistLowerCase|
EventRecord json
   payload          Text.Text
|]


newtype SQLEventBackend m a = S
  { runSql :: R.ReaderT SqlBackend m a
  } deriving (Functor, Applicative, Monad)

instance (MonadIO m) => V.HasEventSource (SQLEventBackend m) where
  readEvents = sqliteReadAll

instance (MonadIO m) => V.HasEventSink (SQLEventBackend m) where
  writeEvent = sqliteWriteEvent


sqliteReadAll :: (A.FromJSON a) => (MonadIO m) => SQLEventBackend m [V.Event a]
sqliteReadAll = S $ do
    xs <- selectList [] []
    -- liftIO $ print (xs :: [Entity EventRecord])
    pure (catMaybes (prep <$> xs))

  where
    prep :: (A.FromJSON a) => Entity EventRecord -> Maybe (V.Event a)
    prep r = A.decode (BL.fromStrict $ encodeUtf8 $ k r)

    k :: Entity EventRecord -> Text.Text
    k t = eventRecordPayload (entityVal t)


  -- runMigration migrateAll

sqliteWriteEvent :: (MonadIO m, A.ToJSON a) => V.Event a -> SQLEventBackend m ()
sqliteWriteEvent v = S $ do
  _ <- insert $ convert v
  pure ()

  where
    convert :: (A.ToJSON a) => V.Event a -> EventRecord
    convert v' = EventRecord (decodeUtf8 $ BL.toStrict $ A.encode v')


prepareDB :: (MonadIO m) => SQLEventBackend m ()
prepareDB = S $ runMigration migrateAll
