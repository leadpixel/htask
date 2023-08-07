{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main (main) where

import qualified Data.Time                      as Time
import qualified Data.UUID.V4                   as UUID
import qualified HTask.CLI.Options              as Opt
import qualified HTask.CLI.Render               as Render
import qualified HTask.CLI.Runners              as Runner
import qualified Leadpixel.Events               as V

import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.IO.Unlift        (MonadUnliftIO)
import           Control.Monad.Random.Class     (MonadRandom (..))
import           Control.Monad.Trans.Class      (lift)
import           Data.Time                      (UTCTime)
import           Data.UUID                      (UUID)
import           Leadpixel.Events.Backends.File
import           Leadpixel.Provider


newtype WrapIO a
  = WrapIO { unwrapIO :: IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadRandom, MonadUnliftIO)

instance Provider UTCTime WrapIO where
  provide = WrapIO Time.getCurrentTime

instance Provider UUID WrapIO where
  provide = WrapIO UUID.nextRandom


newtype App m a
  = App { unApp :: FileEventBackend m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Monad m, MonadUnliftIO m) => V.HasEventSource (App m) where
  readEvents = App V.readEvents

instance (Monad m, MonadUnliftIO m) => V.HasEventSink (App m) where
  writeEvent = App . V.writeEvent

instance (Monad m, Provider k m) => Provider k (App m) where
  provide = App $ lift provide

instance (MonadRandom m) => MonadRandom (App m) where
  getRandom = App $ lift getRandom
  getRandoms = App $ lift getRandoms
  getRandomR = App . lift . getRandomR
  getRandomRs = App . lift . getRandomRs


runApp :: FilePath -> App m a -> m a
runApp file = runFileBackend file . unApp


main :: IO ()
main = do
  options <- Opt.getOptions

  let app = Runner.runAction (Opt.action options)
  x <- unwrapIO $ runApp (Opt.taskfile options) app
  Render.renderResult x
