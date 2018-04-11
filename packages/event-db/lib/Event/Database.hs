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
  ( SQLBackend (..)
  , prepareDB
  , sqliteWriteEvent
  , sqliteReadAll
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


type SQLBackend m = R.ReaderT SqlBackend m

instance (MonadIO m) => V.HasEventSource (SQLBackend m) where
  readEvents = sqliteReadAll

instance (MonadIO m) => V.HasEventSink (SQLBackend m) where
  writeEvent = sqliteWriteEvent


sqliteReadAll :: (A.FromJSON a) => (Monad m, MonadIO m) => SQLBackend m [V.Event a]
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

sqliteWriteEvent :: (Monad m, MonadIO m, A.ToJSON a) => V.Event a -> SQLBackend m ()
sqliteWriteEvent v = do
  _ <- insert $ convert v
  pure ()

  where
    convert :: (A.ToJSON a) => V.Event a -> EventRecord
    convert v' = EventRecord (decodeUtf8 $ BL.toStrict $ A.encode v')


prepareDB :: (MonadIO m) => SQLBackend m ()
prepareDB = runMigration migrateAll
