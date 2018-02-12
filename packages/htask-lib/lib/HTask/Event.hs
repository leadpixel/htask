{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module HTask.Event
  where

import Data.Aeson
import HTask.Capabilities.Time
import HTask.Capabilities.UUID
import qualified Data.UUID as UUID
import Data.Tagged
import GHC.Generics


type EventUUID = UUID.UUID


data Event a = Event
  { eventUuid :: EventUUID
  , timestamp :: Timestamp
  , eventType :: a
  } deriving (Show, Generic)

instance (ToJSON a) => ToJSON (Event a)
instance (FromJSON a) => FromJSON (Event a)


type CanCreateEvent m = (Monad m, CanTime m, CanUuid m)
