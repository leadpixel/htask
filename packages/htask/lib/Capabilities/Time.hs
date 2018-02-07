{-# LANGUAGE FlexibleInstances #-}

module Capabilities.Time
  where

import Control.Monad.Trans.Class
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as SysClock


type Timestamp = Time.UTCTime


class CanTime m where
  now :: m Timestamp

instance CanTime IO where
  now = SysClock.systemToUTCTime <$> SysClock.getSystemTime

instance (MonadTrans m) => CanTime (m IO) where
  now = lift now


zeroTime :: Timestamp
zeroTime = Time.UTCTime (Time.ModifiedJulianDay 0) (Time.secondsToDiffTime 0)
