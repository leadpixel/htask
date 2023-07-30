{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Start
  ( runStart
  ) where

import qualified HTask.Core                as H

import           Data.Text                 (Text)
import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication


runStart :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runStart t
  = formatOutcome <$> runTask (H.startTask t)

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["starting task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"
