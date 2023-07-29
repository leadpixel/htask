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
    formatOutcome x
      = case x of
          API.AddSuccess ref ->
            resultSuccessAdd ref

          API.FailedToAdd ->
            resultError "failed to add"

      where
        resultSuccessAdd ref
          = resultSuccess
              [ "added task: " <> t
              , "ref: " <> H.taskRefText ref
              ]
