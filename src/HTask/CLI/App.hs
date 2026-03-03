{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module HTask.CLI.App
  ( App
  , CanRunAction
  , runApp
  ) where

import qualified Data.Time                  as Time
import qualified Data.UUID.V4               as UUID
import qualified HTask.Core                 as H
import qualified HTask.Events               as V

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.IO.Unlift    (MonadUnliftIO)
import           Control.Monad.Random.Class (MonadRandom (..))
import           Control.Monad.State        (MonadState, get, put)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT (..), ask, runReaderT)
import           Data.IORef
import           Data.Time                  (UTCTime)
import           Data.UUID                  (UUID)
import           HTask.Events
import           HTask.Provider
import           System.Directory           (doesFileExist)
import           System.Exit                (exitFailure)
import           System.IO                  (hFlush, hIsTerminalDevice,
                                             hPutStrLn, stderr, stdout)


newtype App m a
  = App { unApp :: ReaderT (IORef H.TaskMap) (FileEventBackend m) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadUnliftIO)

instance (MonadIO m) => MonadState H.TaskMap (App m) where
  get = App $ do
    ref <- ask
    liftIO $ readIORef ref
  put m = App $ do
    ref <- ask
    liftIO $ writeIORef ref m

type CanRunAction m =
  ( Monad m
  , MonadIO m
  , MonadUnliftIO m
  , MonadRandom m
  , MonadState H.TaskMap m
  , V.HasEventSink m
  , Provider UTCTime m
  , Provider UUID m
  )

instance (Monad m, MonadUnliftIO m) => V.HasEventSource (App m) where
  readEvents = App $ lift V.readEvents

instance (Monad m, MonadUnliftIO m) => V.HasEventSink (App m) where
  writeEvent = App . lift . V.writeEvent

instance (MonadIO m) => Provider UTCTime (App m) where
  provide = App $ liftIO Time.getCurrentTime

instance (MonadIO m) => Provider UUID (App m) where
  provide = App $ liftIO UUID.nextRandom

instance (MonadRandom m) => MonadRandom (App m) where
  getRandom = App $ lift $ lift getRandom
  getRandoms = App $ lift $ lift getRandoms
  getRandomR = App . lift . lift . getRandomR
  getRandomRs = App . lift . lift . getRandomRs


runApp :: (MonadUnliftIO m) => FilePath -> App m a -> m a
runApp file app = do
  liftIO $ ensureFileExists file
  runFileBackend file $ do
    evs <- V.readEvents
    let (initialMap, _) = H.foldEventLog evs
    ref <- liftIO $ newIORef initialMap
    runReaderT (unApp app) ref

  where
    ensureFileExists :: FilePath -> IO ()
    ensureFileExists path = do
      exists <- doesFileExist path
      if exists
        then pure ()
        else do
          isTerm <- hIsTerminalDevice stdout
          if isTerm
            then do
              putStr $ "Task file '" <> path <> "' not found. Create it? [y/N] "
              hFlush stdout
              response <- getLine
              if response `elem` ["y", "Y", "yes", "YES"]
                then writeFile path ""
                else do
                  hPutStrLn stderr "Aborted."
                  exitFailure
            else do
              hPutStrLn stderr $ "Task file '" <> path <> "' not found."
              exitFailure
