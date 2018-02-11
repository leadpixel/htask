module HTask.Capabilities.Time
  where

import qualified Data.Time as Time
import qualified Data.Time.Clock.System as SysClock


type Timestamp = Time.UTCTime


class CanTime m where
  now :: m Timestamp

instance CanTime IO where
  now = SysClock.systemToUTCTime <$> SysClock.getSystemTime


zeroTime :: Timestamp
zeroTime = Time.UTCTime (Time.ModifiedJulianDay 0) (Time.secondsToDiffTime 0)
