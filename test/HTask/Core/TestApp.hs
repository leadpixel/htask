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

import qualified HTask.Core                 as Core
import qualified HTask.Events               as Events

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
import           HTask.Effects

-- We use IORef for mock values to keep everything in ReaderT IO
data TestEnv
  = TestEnv
    { _mockUUIDs :: IORef [UUID]
    , mockTime   :: IORef UTCTime
    , taskMap    :: IORef Core.TaskMap
    }

newtype TestApp m a
  = TestApp { unApp :: ReaderT TestEnv (Events.MemoryEventBackend m) a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (MonadIO m) => MonadState Core.TaskMap (TestApp m) where
  get = TestApp $ do
    env <- ask
    lift $ liftIO $ readIORef (taskMap env)
  put m = TestApp $ do
    env <- ask
    lift $ liftIO $ writeIORef (taskMap env) m

instance (MonadIO m) => Events.HasEventSource (TestApp m) where
  readEvents = TestApp $ lift Events.readEvents

instance (MonadIO m) => Events.HasEventSink (TestApp m) where
  writeEvent = TestApp . lift . Events.writeEvent

instance (MonadIO m) => MonadTime (TestApp m) where
  currentTime = TestApp $ do
    env <- ask
    t <- liftIO $ readIORef (mockTime env)
    -- Auto-increment time slightly on each provide to ensure ordering
    liftIO $ modifyIORef' (mockTime env) (addUTCTime 1)
    pure t

instance (MonadIO m) => MonadUUID (TestApp m) where
  nextUUID = liftIO UUID.nextRandom

runTestApp :: (MonadIO m) => UTCTime -> TestApp m a -> m (a, Seq Lazy.ByteString)
runTestApp time op = do
  uRef <- liftIO $ newIORef [] -- Not using mock UUIDs here
  tRef <- liftIO $ newIORef time
  mRef <- liftIO $ newIORef mempty
  let env = TestEnv uRef tRef mRef
  Events.runMemoryBackend' $ runReaderT (unApp op) env

getResult :: (a, b) -> a
getResult = fst

getLog :: (a, b) -> b
getLog = snd
