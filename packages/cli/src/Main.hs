{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main
  ( main
  ) where

import qualified Data.Time                  as Time
import qualified Data.UUID.V4               as UUID
import qualified HTask.CLI.Options          as Opt
import qualified HTask.CLI.Render           as Render
import qualified HTask.CLI.Runners          as Runner

import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.IO.Unlift    (MonadUnliftIO)
import           Control.Monad.Random.Class (MonadRandom (..))
import           Control.Monad.Trans.Class  (lift)
import           Data.Time                  (UTCTime)
import           Data.UUID                  (UUID)
import           Event.Backend.File
import           Events
import           Leadpixel.Provider


newtype App m a = App { unApp :: FileEventBackend m a }
  deriving (Functor, Applicative, Monad, MonadIO, HasEventSource, HasEventSink)

instance (Monad m, Provider k m) => Provider k (App m) where
  provide = App $ lift provide

instance (MonadRandom m) => MonadRandom (App m) where
  getRandom = App $ lift getRandom
  getRandoms = App $ lift getRandoms
  getRandomR = App . lift . getRandomR
  getRandomRs = App . lift . getRandomRs


-- TODO: remove orphans
instance Provider UTCTime WrapIO where
  provide = WrapIO Time.getCurrentTime

instance Provider UUID WrapIO where
  provide = WrapIO UUID.nextRandom

runApp :: FilePath -> App m a -> m a
runApp file app = runFileBackend file (unApp app)


newtype WrapIO a = WrapIO { unwrapIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadRandom)


main :: IO ()
main = do
  options <- Opt.getOptions

  let app = Runner.runAction (Opt.action options)
  x <- unwrapIO $ runApp (Opt.taskfile options) app
  Render.renderResult x
