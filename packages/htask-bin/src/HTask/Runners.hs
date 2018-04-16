{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners
  ( runCommand
  ) where

import qualified HTask as H
import HTask.Actions
import HTask.Config
import HTask.Output
import HTask.Runners.Summary
import HTask.Runners.List
import HTask.Runners.Add
import HTask.Runners.Start
import HTask.Runners.Stop
import HTask.Runners.Complete
import HTask.Runners.Remove
import HTask.Runners.Pick
import HTask.Runners.Done
import HTask.Runners.Drop
import HTask.TaskApplication
import qualified Control.Monad.Reader as R
import Control.Monad.IO.Class
import Event


runCommand :: Options -> IO ()
runCommand opts
  = R.runReaderT (runFileBackend $ runAction $ action opts) (taskfile opts)
  >>= renderDocument (formatter opts)


-- runWithFile :: (MonadIO m) => FileBackend m a -> TaskConfig m a
-- runWithFile (F x) = R.withReaderT taskfile x


runAction :: (MonadIO m, H.CanCreateTask m, H.HasTasks (TaskApplication m)) => Action -> EventBackend m Document
runAction Summary        = runSummary
runAction (List d k)     = runList d k
runAction (Add tex)      = runAdd tex
runAction (Start ref)    = runStart ref
runAction (Stop ref)     = runStop ref
runAction (Complete ref) = runComplete ref
runAction (Remove ref)   = runRemove ref
runAction Pick           = runPick
runAction Drop           = runDrop
runAction Done           = runDone
