{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Add
  ( runAdd
  ) where

import qualified HTask.API             as API
import qualified HTask.Task            as H

import           HTask.Output.Document
import           HTask.TaskApplication

import           Data.Semigroup        ((<>))
import           Data.Text             (Text)


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
