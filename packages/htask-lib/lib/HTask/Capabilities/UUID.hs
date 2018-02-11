module HTask.Capabilities.UUID
  where

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID


class CanUuid m where
  uuidGen :: m UUID.UUID

instance CanUuid IO where
  uuidGen = UUID.nextRandom
