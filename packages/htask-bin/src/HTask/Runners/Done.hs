{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Done
  ( runDone
  ) where

import HTask.TaskApplication
import HTask.Output
import qualified HTask as H
import qualified Data.Text as Text
import Data.Semigroup ((<>))


type DoneOutput = [(H.Task, Either String H.TaskRef)]


runDone :: TaskConfig IO Document
runDone
  = formatOutcome <$> runTask doneTask

  where

    doneTask :: TaskApplication IO DoneOutput
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
