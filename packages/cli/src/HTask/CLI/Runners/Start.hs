{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Start
  ( runStart
  ) where

import qualified HTask.Core.API             as API
import qualified HTask.Core.Task            as H

import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication

import           Data.Semigroup        ((<>))
import           Data.Text             (Text)


runStart :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runStart t
  = formatOutcome <$> runTask (API.startTask t)

  where
    formatOutcome x
      = case x of
          API.ModifySuccess tsk ->
            resultSuccess ["starting task: " <> H.description tsk]

          API.FailedToFind ->
            resultError "unable to find matching task"

          API.FailedToModify ->
            resultError "unable to modify matching task"
