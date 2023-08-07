{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Remove (runRemove) where

import qualified HTask.Core                as H

import           Data.Text                 (Text)
import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication


runRemove :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runRemove t
  = formatOutcome <$> runTask (H.removeTask t)

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["removing task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"
