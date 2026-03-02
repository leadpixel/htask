{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HTask.Core.TestApp
  ( TestApp
  , getLog
  , getResult
  , runTestApp
  ) where

import qualified HTask.Core                as H
import qualified HTask.Events              as Memory
import qualified HTask.Events              as V

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.State       (MonadState)
import qualified Control.Monad.State       as State
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, runStateT)
import qualified Data.ByteString.Lazy      as Lazy
import           Data.Sequence             (Seq)
import           Data.Time                 (UTCTime)
import           Data.UUID                 (UUID)
import qualified Data.UUID.V4              as UUID
import           HTask.Provider


newtype TestApp m a
  = TestApp { unApp :: StateT (UTCTime, H.TaskMap) (Memory.MemoryEventBackend m) a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Monad m) => MonadState H.TaskMap (TestApp m) where
  get = TestApp $ State.gets snd
  put m = TestApp $ State.modify (\(t, _) -> (t, m))

instance (Monad m) => V.HasEventSource (TestApp m) where
  readEvents = TestApp $ lift V.readEvents

instance (Monad m) => V.HasEventSink (TestApp m) where
  writeEvent = TestApp . lift . V.writeEvent

instance (Monad m) => Provider UTCTime (TestApp m) where
  provide = TestApp $ State.gets fst

instance (MonadIO m) => Provider UUID (TestApp m) where
  provide = liftIO UUID.nextRandom


runTestApp :: (Monad m) => UTCTime -> TestApp m a -> m (a, Seq Lazy.ByteString)
runTestApp time op = do
  ((res, _), eventLog) <- Memory.runMemoryBackend' $ runStateT (unApp op) (time, mempty)
  pure (res, eventLog)


getResult :: (a, b) -> a
getResult = fst

getLog :: (a, b) -> b
getLog = snd
