module HTask.Runners
  ( runCommand
  ) where

import qualified HTask as H
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Trans  as T

import HTask.Actions
import HTask.Config
import HTask.Output
import HTask.TaskApplication

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
import Event
import Event.Database


runCommand :: Options -> IO ()
runCommand opts
  = R.runReaderT (runFileBackend $ runAction $ action opts) (taskfile opts)
  >>= renderDocument (formatter opts)


runAction
  :: (HasEventBackend m, H.CanCreateTask m, CanRandom m)
  => Action -> m Document
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
