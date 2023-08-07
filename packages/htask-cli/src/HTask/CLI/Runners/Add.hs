{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Add (runAdd) where

import qualified HTask.Core                as H

import           Data.Text                 (Text)
import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication


runAdd :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runAdd t
  = formatOutcome <$> runTask (H.addTask t)

  where
    formatOutcome (H.AddSuccess ref)
      = resultSuccessAdd ref

    formatOutcome H.FailedToAdd
      = resultError "failed to add"

    resultSuccessAdd ref
      = resultSuccess
          [ "added task: " <> t
          , "ref: " <> H.taskUuidToText ref
          ]
