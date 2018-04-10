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
  ( PostgresBackend (..)
  , prepareDB
  , sqliteWriteEvent
  , sqliteReadAll
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text.Encoding
import Database.Persist
import Database.Persist.Postgresql
-- import Database.Persist.Sqlite as S
import Database.Persist.TH

import qualified Control.Monad.Reader         as R
import qualified Data.Aeson                   as A
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Text                    as Text
import qualified Event                        as V


newtype PostgresBackend a = P (R.ReaderT ConnectionString IO a)

instance V.HasEventSource PostgresBackend where
  readEvents = postgresReadEvents

instance V.HasEventSink PostgresBackend where
  writeEvent = postgresWriteEvent


postgresReadEvents :: (A.FromJSON a) => PostgresBackend [V.Event a]
postgresReadEvents
 = undefined


postgresWriteEvent :: (A.ToJSON a) => V.Event a -> PostgresBackend ()
postgresWriteEvent
 = undefined


share [mkMigrate "migrateAll", mkPersist sqlSettings] [persistLowerCase|
EventRecord json
   payload          Text.Text
|]


sqliteReadAll :: (A.FromJSON a) => (Monad m, MonadIO m) => R.ReaderT SqlBackend m [V.Event a]
sqliteReadAll = do
    xs <- selectList [] []
    -- liftIO $ print (xs :: [Entity EventRecord])
    pure (catMaybes (prep <$> xs))

  where
    prep :: (A.FromJSON a) => Entity EventRecord -> Maybe (V.Event a)
    prep r = A.decode (BL.fromStrict $ encodeUtf8 $ k r)

    k :: Entity EventRecord -> Text.Text
    k t = eventRecordPayload (entityVal t)


  -- runMigration migrateAll

sqliteWriteEvent :: (Monad m, MonadIO m, A.ToJSON a) => V.Event a -> R.ReaderT SqlBackend m ()
sqliteWriteEvent v = do
  _ <- insert $ convert v
  pure ()

  where
    convert :: (A.ToJSON a) => V.Event a -> EventRecord
    convert v' = EventRecord (decodeUtf8 $ BL.toStrict $ A.encode v')


prepareDB :: (MonadIO m) => R.ReaderT SqlBackend m ()
prepareDB = runMigration migrateAll
