{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Drop
  ( runDrop
  ) where


import qualified Data.UUID             as UUID
import qualified HTask.API             as API
import qualified HTask.Task            as H

import           HTask.Output.Document
import           HTask.TaskApplication

import           Data.Semigroup        ((<>))
import           Data.Tagged           (untag)
import           Data.Text             (Text)


inProgress :: H.Task -> Bool
inProgress t = H.status t == H.InProgress


taskRefText :: H.Task -> Text
taskRefText = UUID.toText . untag . H.taskRef


runDrop :: (HasEventBackend m, H.CanCreateTask m) => m RunResult
runDrop
  = formatOutcome <$> runTask dropTask

  where
    dropTask
      = (filter inProgress <$> API.listTasks) >>= mapM (API.stopTask . taskRefText)

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
