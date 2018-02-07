{-# LANGUAGE FlexibleInstances #-}

module Capabilities.UUID
  where

import Control.Monad.Trans.Class
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID


class CanUuid m where
  uuidGen :: m UUID.UUID

instance CanUuid IO where
  uuidGen = UUID.nextRandom

instance (MonadTrans m) => CanUuid (m IO) where
  uuidGen = lift uuidGen
