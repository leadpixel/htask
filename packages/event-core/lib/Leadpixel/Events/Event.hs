{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Leadpixel.Events.Event
  ( Event (..)
  , createEvent
  ) where

import           Data.Aeson
import           Data.Time
import           GHC.Generics

data Event a = Event
  { timestamp :: UTCTime
  , payload   :: a
  } deriving (Show, Eq, Generic)


instance (ToJSON a) => ToJSON (Event a)
instance (FromJSON a) => FromJSON (Event a)


createEvent :: (Functor m) => m UTCTime -> a -> m (Event a)
createEvent readTime x = (\t -> Event { timestamp = t , payload = x }) <$> readTime
