{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module HTask.CLI.App
  ( App
  , runApp
  ) where

import qualified Data.Time                      as Time
import qualified Data.UUID.V4                   as UUID
import qualified HTask.Core                     as H
import qualified Leadpixel.Events               as V

import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.IO.Unlift        (MonadUnliftIO)
import           Control.Monad.Random.Class     (MonadRandom (..))
import           Control.Monad.State            (MonadState)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State      (StateT, evalStateT)
import           Data.Time                      (UTCTime)
import           Data.UUID                      (UUID)
import           Leadpixel.Events.Backends.File
import           Leadpixel.Provider


newtype App m a
  = App { unApp :: StateT H.TaskMap (FileEventBackend m) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadState H.TaskMap)

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
runApp file app
  = runFileBackend file
  $ V.readEvents >>= evalStateT (unApp app) . (fst . H.foldEventLog)
