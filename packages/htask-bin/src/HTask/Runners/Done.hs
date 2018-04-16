{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Done
  ( runDone
  ) where

import Control.Monad.IO.Class
import Event
import HTask.TaskApplication
import HTask.Output
import qualified HTask as H
import qualified Data.Text as Text
import Data.Semigroup ((<>))


type DoneOutput = [(H.Task, Either String H.TaskRef)]


runDone :: (H.HasTasks (TaskApplication m), H.CanCreateTask m, MonadIO m) => EventBackend m Document
runDone
  = formatOutcome <$> runTask doneTask

  where

    doneTask :: (CanUuid m, CanTime m,  Monad m, H.HasTasks m, HasEventSink m) => m DoneOutput
    doneTask = do
      xs <- H.listTasks
      let ts = filter isCurrent xs
      mapM execDone ts

    execDone t = do
      x <- H.completeTask (H.taskRef t)
      pure (t, x)

    isCurrent t = H.status t == H.InProgress

    formatOutcome :: DoneOutput -> Document
    formatOutcome xs
      = mconcat (formatDone <$> xs)

    formatDone (_t, Left err) = formatError (Text.pack err)
    formatDone (t, Right _ref) = formatSuccessComplete t

    formatSuccessComplete tx
      = formatSuccess ("completing task: " <> H.description tx)
