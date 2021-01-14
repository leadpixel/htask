{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Complete
  ( runComplete
  ) where

import qualified HTask.Core.API            as API
import qualified HTask.Core.Task           as H

import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication

import           Data.Text                 (Text)


runComplete :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runComplete t
  = formatOutcome <$> runTask (API.completeTask t)

  where
    formatOutcome x
      = case x of
          API.ModifySuccess tsk ->
            resultSuccess ["completing task: " <> H.description tsk]

          API.FailedToFind ->
            resultError "unable to find matching task"

          API.FailedToModify ->
            resultError "unable to modify matching task"
