{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Trans        as T
import qualified HTask.CLI.Options          as Opt
import qualified HTask.CLI.Render           as Render
import qualified HTask.CLI.Runners          as Runner

import Data.Time
import Data.UUID
import           Control.Monad.Random.Class
import           Event.Backend.File
import           Leadpixel.Provider


newtype Thing a = Thing (FileEventBackend IO a)
  deriving (Functor, Applicative, Monad)

-- instance Provider UTCTime Thing where
--   gen = Thing $ T.lift gen

-- instance Provider UUID Thing where
--   gen = Thing $ T.lift gen

instance MonadRandom Thing where
  getRandom = Thing $ T.lift getRandom
  getRandoms = Thing $ T.lift getRandoms
  getRandomR = Thing . T.lift . getRandomR
  getRandomRs = Thing . T.lift . getRandomRs


main :: IO ()
main = do
  options <- Opt.getOptions
  let op = Runner.runAction (Opt.action options)
  runFileBackend (Opt.taskfile options) op >>= Render.renderResult
