module Leadpixel.Events.Backends
  ( HasEventSource (..)
  , HasEventSink (..)
  ) where

import qualified Data.Aeson             as Aeson
import qualified Leadpixel.Events.Event as V


class (Monad m) => HasEventSource m where
  readEvents :: (Aeson.FromJSON a) => m [V.Event a]

class (Monad m) => HasEventSink m where
  writeEvent :: (Aeson.ToJSON a) => V.Event a -> m ()
  writeEvents :: (Aeson.ToJSON a) => [V.Event a] -> m ()
  writeEvents = defaultWriteEvents


defaultWriteEvents :: (Monad m, HasEventSink m, Aeson.ToJSON a) => [V.Event a] -> m ()
defaultWriteEvents = mapM_ writeEvent
