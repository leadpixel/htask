{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Stop
  ( runStop
  ) where

import qualified HTask.Core                as H

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Text                 (Text)
import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication


runStop :: (MonadIO m, HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runStop t
  = formatOutcome <$> runTask (H.stopTask t)

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["stopping task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"
