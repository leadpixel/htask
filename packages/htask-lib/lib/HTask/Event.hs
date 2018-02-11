{-# LANGUAGE ConstraintKinds #-}

module HTask.Event
  where

import HTask.Capabilities.Time
import HTask.Capabilities.UUID
import qualified Data.UUID as UUID
import Data.Tagged


type EventUUID = UUID.UUID


data Event a = Event
  { eventUuid :: EventUUID
  , timestamp :: Timestamp
  , eventType :: a
  } deriving (Show)


type CanCreateEvent m = (Monad m, CanTime m, CanUuid m)
