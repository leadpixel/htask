{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module HTask.Runners
  ( runAction
  ) where

import qualified HTask as H
import qualified Event as V

import HTask.Actions
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
