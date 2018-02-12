{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module HTask.Event
  where

import Data.Aeson
import Data.Tagged
import GHC.Generics
import HTask.Capabilities.Time
import HTask.Capabilities.UUID
import qualified Data.UUID as UUID


data EventIdent = EventIdent
type EventUUID = Tagged EventIdent UUID.UUID


data Event a = Event
  { eventUuid :: EventUUID
  , timestamp :: Timestamp
  , eventType :: a
  } deriving (Show, Generic)


instance (ToJSON a) => ToJSON (Event a)
instance (FromJSON a) => FromJSON (Event a)


type CanCreateEvent m = (Monad m, CanTime m, CanUuid m)


createEvent :: (CanCreateEvent m) => a -> m (Event a)
createEvent x
  = (\u m -> Event u m x)
  <$> (Tagged <$> uuidGen)
  <*> now
