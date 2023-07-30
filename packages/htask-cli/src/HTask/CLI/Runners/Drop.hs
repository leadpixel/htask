{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Drop
  ( runDrop
  ) where


import qualified Data.UUID                 as UUID
import qualified HTask.Core.API            as API
import qualified HTask.Core.Task           as H

import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication

import           Data.Tagged               (untag)
import           Data.Text                 (Text)


inProgress :: H.Task -> Bool
inProgress t = H.status t == H.InProgress


taskUuidText :: H.Task -> Text
taskUuidText = UUID.toText . untag . H.taskUuid


runDrop :: (HasEventBackend m, H.CanCreateTask m) => m RunResult
runDrop
  = formatOutcome <$> runTask dropTask

  where
    dropTask
      = API.listTasks >>= mapM (API.stopTask . taskUuidText) . filter inProgress

    formatOutcome
      = resultSuccess . fmap formatRow

    formatRow x
      = case x of
          API.ModifySuccess tsk ->
            "stopping task: " <> H.description tsk

          API.FailedToFind ->
            "unable to find matching task"

          API.FailedToModify ->
            "unable to modify matching task"
