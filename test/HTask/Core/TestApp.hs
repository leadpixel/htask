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

import qualified HTask.Core                 as H
import qualified HTask.Events               as V

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.State        (MonadState)
import qualified Control.Monad.State        as State
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Data.ByteString.Lazy       as Lazy
import           Data.IORef
import           Data.Sequence              (Seq)
import           Data.Time                  (UTCTime, addUTCTime)
import           Data.UUID                  (UUID)
import qualified Data.UUID.V4               as UUID
import           HTask.Provider


-- We use IORef for mock values to keep everything in ReaderT IO
data TestEnv
  = TestEnv
    { _mockUUIDs :: IORef [UUID]
    , mockTime   :: IORef UTCTime
    , taskMap    :: IORef H.TaskMap
    }

newtype TestApp m a
  = TestApp { unApp :: ReaderT TestEnv (V.MemoryEventBackend m) a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (MonadIO m) => MonadState H.TaskMap (TestApp m) where
  get = TestApp $ do
    env <- ask
    lift $ liftIO $ readIORef (taskMap env)
  put m = TestApp $ do
    env <- ask
    lift $ liftIO $ writeIORef (taskMap env) m

instance (MonadIO m) => V.HasEventSource (TestApp m) where
  readEvents = TestApp $ lift V.readEvents

instance (MonadIO m) => V.HasEventSink (TestApp m) where
  writeEvent = TestApp . lift . V.writeEvent

instance (MonadIO m) => Provider UTCTime (TestApp m) where
  provide = TestApp $ do
    env <- ask
    t <- liftIO $ readIORef (mockTime env)
    -- Auto-increment time slightly on each provide to ensure ordering
    liftIO $ modifyIORef' (mockTime env) (addUTCTime 1)
    pure t

instance (MonadIO m) => Provider UUID (TestApp m) where
  provide = liftIO UUID.nextRandom


runTestApp :: (MonadIO m) => UTCTime -> TestApp m a -> m (a, Seq Lazy.ByteString)
runTestApp time op = do
  uRef <- liftIO $ newIORef [] -- Not using mock UUIDs here
  tRef <- liftIO $ newIORef time
  mRef <- liftIO $ newIORef mempty
  let env = TestEnv uRef tRef mRef
  V.runMemoryBackend' $ runReaderT (unApp op) env


getResult :: (a, b) -> a
getResult = fst

getLog :: (a, b) -> b
getLog = snd
