{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module HTask.CLI.TestApp
  ( TestApp
  , runTestApp
  ) where

import qualified HTask.Core                 as H
import qualified HTask.Events               as V

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.IO.Unlift    (MonadUnliftIO)
import           Control.Monad.Random.Class (MonadRandom (..))
import           Control.Monad.State        (MonadState, get, put)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.IORef
import           Data.Time                  (UTCTime)
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
import           HTask.Events               (runFileBackend)
import           HTask.Provider


-- We use IORef for mock values to keep everything in ReaderT IO
data TestEnv
  = TestEnv
    { mockUUIDs :: IORef [UUID]
    , mockTime  :: UTCTime
    , taskMap   :: IORef H.TaskMap
    }

newtype TestApp a
  = TestApp { unTestApp :: ReaderT TestEnv IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadUnliftIO)

instance MonadState H.TaskMap TestApp where
  get = TestApp $ do
    env <- ask
    liftIO $ readIORef (taskMap env)
  put m = TestApp $ do
    env <- ask
    liftIO $ writeIORef (taskMap env) m

instance V.HasEventSource TestApp where
  readEvents = pure []

instance V.HasEventSink TestApp where
  writeEvent _ = pure ()

instance Provider UTCTime TestApp where
  provide = TestApp $ mockTime <$> ask

instance Provider UUID TestApp where
  provide = TestApp $ do
    env <- ask
    liftIO $ atomicModifyIORef' (mockUUIDs env) $ \uuids ->
      case uuids of
        []     -> ([], UUID.nil)
        (u:us) -> (us, u)

instance MonadRandom TestApp where
  getRandom = TestApp $ liftIO getRandom
  getRandoms = TestApp $ liftIO getRandoms
  getRandomR = TestApp . liftIO . getRandomR
  getRandomRs = TestApp . liftIO . getRandomRs


runTestApp :: FilePath -> [UUID] -> UTCTime -> TestApp a -> IO a
runTestApp file uuids time app = do
  uRef <- newIORef uuids
  -- We still use runFileBackend to get the initial events
  evs <- runFileBackend file V.readEvents
  mRef <- newIORef (fst (H.foldEventLog evs))
  let env = TestEnv uRef time mRef
  runReaderT (unTestApp app) env
