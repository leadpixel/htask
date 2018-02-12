module HTask.Capabilities.Time
  ( Timestamp
  , CanTime (now)
  ) where

import qualified Data.Time as Time
import qualified Data.Time.Clock.System as SysClock


type Timestamp = Time.UTCTime


class CanTime m where
  now :: m Timestamp

instance CanTime IO where
  now = SysClock.systemToUTCTime <$> SysClock.getSystemTime
