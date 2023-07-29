{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Stop
  ( runStop
  ) where

import qualified HTask.Core.API            as API
import qualified HTask.Core.Task           as H

import           Control.Monad.IO.Class    (MonadIO)
import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication

import           Data.Text                 (Text)


runStop :: (MonadIO m, HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runStop t
  = formatOutcome <$> runTask (API.stopTask t)

  where
    formatOutcome x
      = case x of
          API.ModifySuccess tsk ->
            resultSuccess ["stopping task: " <> H.description tsk]

          API.FailedToFind ->
            resultError "unable to find matching task"

          API.FailedToModify ->
            resultError "unable to modify matching task"
