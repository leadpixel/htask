{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Complete (runComplete) where

import qualified HTask.Core                as H

import           Data.Text                 (Text)
import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication


runComplete :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runComplete t
  = formatOutcome <$> runTask (H.completeTask t)

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["completing task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"
