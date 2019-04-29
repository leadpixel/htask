{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Trans    as T
import qualified Event                  as V
import qualified HTask.CLI              as CLI
import qualified HTask.Config           as Config
import qualified HTask.Runners          as Runner

import           Event.Backend.File
import           HTask.Output.Renderers

instance (Monad m, V.CanTime m) => V.CanTime (FileBackend m) where
  now = T.lift V.now

instance (Monad m, V.CanUuid m) => V.CanUuid (FileBackend m) where
  uuidGen = T.lift V.uuidGen

instance (Monad m, V.CanRandom m) => V.CanRandom (FileBackend m) where
  getRandomRange = T.lift . V.getRandomRange


main :: IO ()
main = do
  options <- CLI.getOptions
  let op = Runner.runAction (Config.action options)
  runFileBackend op (Config.taskfile options) >>= renderResult
