{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Add
  ( runAdd
  ) where

import qualified HTask.Core.API            as API
import qualified HTask.Core.Task           as H

import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication

import           Data.Text                 (Text)


runAdd :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runAdd t
  = formatOutcome <$> runTask (API.addTask t)

  where
    formatOutcome (API.AddSuccess ref)
      = resultSuccessAdd ref

    formatOutcome API.FailedToAdd
      = resultError "failed to add"

    resultSuccessAdd ref
      = resultSuccess
          [ "added task: " <> t
          , "ref: " <> H.taskUuidToText ref
          ]
