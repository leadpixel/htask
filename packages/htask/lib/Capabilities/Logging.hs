{-# LANGUAGE FlexibleInstances #-}

module Capabilities.Logging
  where

import Control.Monad.Trans.Class


class CanLog m where
  logDebug :: (Show a) => a -> m ()
  logWarning :: (Show a) => a -> m ()
  logError :: (Show a) => a -> m ()

instance CanLog IO where
  logDebug = print . show
  logWarning = print . show
  logError = print . show

instance (MonadTrans t) => CanLog (t IO) where
  logDebug = lift . logDebug
  logWarning = lift . logWarning
  logError = lift . logError
