module Capabilities.Random
  ( CanRandom (getRandomRange)
  ) where

import qualified System.Random as R


class (Monad m) => CanRandom m where
  getRandomRange :: (R.Random a) => (a, a) -> m a

instance CanRandom IO where
  getRandomRange = R.randomRIO
