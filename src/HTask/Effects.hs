{-# LANGUAGE FlexibleInstances #-}

module HTask.Effects
  ( MonadTime (..)
  , MonadUUID (..)
  ) where

import           Data.Time    (UTCTime, getCurrentTime)
import           Data.UUID    (UUID)
import qualified Data.UUID.V4 as UUID


class (Monad m) => MonadTime m where
  currentTime :: m UTCTime

instance MonadTime IO where
  currentTime = getCurrentTime


class (Monad m) => MonadUUID m where
  nextUUID :: m UUID

instance MonadUUID IO where
  nextUUID = UUID.nextRandom
