{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Trans        as T
import qualified HTask.CLI.Options          as Opt
import qualified HTask.CLI.Render           as Render
import qualified HTask.CLI.Runners          as Runner

import           Control.Monad.Random.Class
import           Event.Backend.File
import           Leadpixel.Provider


-- instance (Monad m, F.CanTime m, T.MonadTrans t) => F.CanTime (t m) where
--   gen = T.lift F.gen

-- instance (Monad m, F.CanUuid m, T.MonadTrans t) => F.CanUuid (t m) where
--   gen = T.lift F.gen

-- instance (Monad m, F.CanRandom m, T.MonadTrans t) => F.CanRandom (t m) where
--   gen = T.lift . F.gen

instance (Provider k IO) => Provider k (FileEventBackend IO) where
  gen = T.lift gen

instance MonadRandom (FileEventBackend IO) where
  getRandom = T.lift getRandom
  getRandoms = T.lift getRandoms
  getRandomR = T.lift . getRandomR
  getRandomRs = T.lift . getRandomRs


main :: IO ()
main = do
  options <- Opt.getOptions
  let op = Runner.runAction (Opt.action options)
  runFileBackend (Opt.taskfile options) op >>= Render.renderResult
