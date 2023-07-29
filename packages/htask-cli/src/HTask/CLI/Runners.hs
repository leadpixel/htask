{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}


module HTask.CLI.Runners
  ( runAction
  ) where

import           Control.Monad.Random.Class
import qualified HTask.Core.Task            as H

import           HTask.CLI.Actions
import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication

import           HTask.CLI.Runners.Add
import           HTask.CLI.Runners.Complete
import           HTask.CLI.Runners.Done
import           HTask.CLI.Runners.Drop
import           HTask.CLI.Runners.List
import           HTask.CLI.Runners.Pick
import           HTask.CLI.Runners.Remove
import           HTask.CLI.Runners.Start
import           HTask.CLI.Runners.Stop
import           HTask.CLI.Runners.Summary


runAction
  :: (HasEventBackend m, H.CanCreateTask m, MonadRandom m)
  => Action -> m RunResult
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
