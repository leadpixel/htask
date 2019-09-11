{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Trans as T
import qualified Effects             as F
import qualified HTask.CLI           as CLI
import qualified HTask.Render        as Render
import qualified HTask.Runners       as Runner

import           Event.Backend.File


instance (Monad m, F.CanTime m, T.MonadTrans t) => F.CanTime (t m) where
  now = T.lift F.now

instance (Monad m, F.CanUuid m, T.MonadTrans t) => F.CanUuid (t m) where
  uuidGen = T.lift F.uuidGen

instance (Monad m, F.CanRandom m, T.MonadTrans t) => F.CanRandom (t m) where
  getRandomRange = T.lift . F.getRandomRange


main :: IO ()
main = do
  options <- CLI.getOptions
  let op = Runner.runAction (CLI.action options)
  runFileBackend (CLI.taskfile options) op >>= Render.renderResult
