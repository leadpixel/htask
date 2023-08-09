{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Leadpixel.Events
  ( Event (..)
  , HasEventSink (..)
  , HasEventSource (..)
  , createEvent
  ) where

import qualified Data.Aeson         as Aeson

import           Data.Time          (UTCTime)
import           GHC.Generics       (Generic)
import           Leadpixel.Provider


class (Monad m) => HasEventSource m where
  readEvents :: (Aeson.FromJSON a) => m [Event a]


class (Monad m) => HasEventSink m where
  writeEvent :: (Aeson.ToJSON a) => Event a -> m ()

  writeEvents :: (Aeson.ToJSON a) => [Event a] -> m ()
  writeEvents = defaultWriteEvents


defaultWriteEvents :: (Monad m, HasEventSink m, Aeson.ToJSON a) => [Event a] -> m ()
defaultWriteEvents = mapM_ writeEvent


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
