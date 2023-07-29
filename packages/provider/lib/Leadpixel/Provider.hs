{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Leadpixel.Provider
  ( Provider (provide)
  , providing
  , providingM
  ) where

import qualified Control.Monad.Trans.Reader as Reader

import           Control.Monad.Trans.Reader (ReaderT, runReaderT)


class (Monad m) => Provider r m where
  provide :: m r

instance Provider k ((->) k) where
  provide = id

instance (Monad m) => Provider k (ReaderT k m) where
  provide = Reader.ask


providing :: (Monad m) => r -> ReaderT r m a -> m a
providing = flip runReaderT


providingM :: (Monad m) => m r -> ReaderT r m a -> m a
providingM gen op = gen >>= runReaderT op
