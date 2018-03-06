{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners
  ( runCommand
  ) where

import Data.Semigroup ((<>))
import Data.Tagged
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
import qualified Data.Text              as Text
import qualified HTask as H


runCommand :: Options -> IO ()
runCommand opts
  = R.runReaderT (runAction $ action opts) (globals opts)
  >>= renderDocument (formatter $ globals opts)


runAction :: Action -> TaskConfig Document
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
