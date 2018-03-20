{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Event.Event
  ( Event (..)
  , CanCreateEvent
  , createEvent
  ) where

import Data.Aeson
import Data.Tagged
import GHC.Generics
import Capabilities
import qualified Data.UUID as UUID


type EventIdent = ()
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
