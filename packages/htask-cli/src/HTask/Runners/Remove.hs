{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Remove
  ( runRemove
  ) where

import qualified HTask.API             as API
import qualified HTask.Task            as H

import           HTask.Output.Document
import           HTask.TaskApplication

import           Data.Semigroup        ((<>))
import           Data.Text             (Text)


runRemove :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runRemove t
  = formatOutcome <$> runTask (API.removeTask t)

  where
    formatOutcome x
      = case x of
          API.ModifySuccess tsk ->
            resultSuccess ["removing task: " <> H.description tsk]

          API.FailedToFind ->
            resultError "unable to find matching task"

          API.FailedToModify ->
            resultError "unable to modify matching task"
