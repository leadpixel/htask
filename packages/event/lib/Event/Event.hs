{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Event.Event
  ( Event (..)
  , CanCreateEvent
  , createEvent
  ) where

import Data.Aeson
import GHC.Generics
import Capabilities
import qualified Data.UUID as UUID


type EventIdent = ()


data Event a = Event
  { timestamp :: Timestamp
  , eventType :: a
  } deriving (Show, Generic)


instance (ToJSON a) => ToJSON (Event a)
instance (FromJSON a) => FromJSON (Event a)


type CanCreateEvent m = (Monad m, CanTime m)


createEvent :: (CanCreateEvent m) => a -> m (Event a)
createEvent x
  = (`Event` x)
  <$> now
