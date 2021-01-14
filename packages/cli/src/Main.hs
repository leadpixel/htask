{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Trans        as T
import qualified HTask.CLI.Options          as Opt
import qualified HTask.CLI.Render           as Render
import qualified HTask.CLI.Runners          as Runner

import           Control.Monad.IO.Unlift
import           Control.Monad.Random.Class
import           Event.Backend.File
import           Events
import           Leadpixel.Provider


newtype App m a = App { runApp :: FileEventBackend m a }
  deriving (Functor, Applicative, Monad)

instance (Monad m, MonadUnliftIO m) => HasEventSource (App m) where
  readEvents = App readEvents

instance (Monad m, MonadUnliftIO m) => HasEventSink (App m) where
  writeEvent = App . writeEvent

instance (Provider k m) => Provider k (App m) where
  gen = App $ T.lift gen

instance (MonadRandom m) => MonadRandom (App m) where
  getRandom = App $ T.lift getRandom
  getRandoms = App $ T.lift getRandoms
  getRandomR = App . T.lift . getRandomR
  getRandomRs = App . T.lift . getRandomRs


main :: IO ()
main = do
  options <- Opt.getOptions
  let app = Runner.runAction (Opt.action options)
  runFileBackend (Opt.taskfile options) (runApp app) >>= Render.renderResult
