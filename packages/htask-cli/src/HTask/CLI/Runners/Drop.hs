{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Drop (runDrop) where

import qualified Data.Sequence             as Seq
import qualified HTask.Core                as H

import           Data.Foldable
import           Data.Text                 (Text)
import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication


inProgress :: H.Task -> Bool
inProgress t = H.status t == H.InProgress


taskToText :: H.Task -> Text
taskToText = H.taskUuidToText . H.taskUuid


runDrop :: (HasEventBackend m, H.CanCreateTask m) => m RunResult
runDrop
  = formatOutcome <$> runTask dropTask

  where
    dropTask
      = H.listTasks >>= mapM (H.stopTask . taskToText) . Seq.filter inProgress

    formatOutcome
      = resultSuccess . toList . fmap formatRow

    formatRow x
      = case x of
          H.ModifySuccess tsk ->
            "stopping task: " <> H.description tsk

          H.FailedToFind ->
            "unable to find matching task"

          H.FailedToModify ->
            "unable to modify matching task"
