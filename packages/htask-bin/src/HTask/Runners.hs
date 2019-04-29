{-# LANGUAGE ConstraintKinds #-}


module HTask.Runners
  ( runAction
  ) where

import qualified Event                  as V
import qualified HTask                  as H

import           HTask.Actions
import           HTask.Output
import           HTask.TaskApplication

import           HTask.Runners.Add
import           HTask.Runners.Complete
import           HTask.Runners.Done
import           HTask.Runners.Drop
import           HTask.Runners.List
import           HTask.Runners.Pick
import           HTask.Runners.Remove
import           HTask.Runners.Start
import           HTask.Runners.Stop
import           HTask.Runners.Summary


runAction
  :: (HasEventBackend m, H.CanCreateTask m, V.CanRandom m)
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
