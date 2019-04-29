{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Done
  ( runDone
  ) where

import           Data.Semigroup        ((<>))
import qualified Data.Text             as Text
import           Event
import qualified HTask                 as H
import           HTask.Output
import           HTask.TaskApplication


type DoneOutput = [(H.Task, Either String H.TaskRef)]


runDone :: (HasEventBackend m, H.CanCreateTask m) => m RunResult
runDone
  = formatOutcome <$> runTask doneTask

  where

    doneTask :: (CanUuid m, CanTime m,  Monad m, H.HasTasks m, HasEventSink m) => m DoneOutput
    doneTask = do
      xs <- H.listTasks
      let ts = filter isCurrent xs
      mapM execDone ts

    execDone t = (\x -> (t, x)) <$> H.completeTask (H.taskRef t)

    isCurrent t = H.status t == H.InProgress

    formatOutcome :: DoneOutput -> RunResult
    formatOutcome xs
      = undefined (formatDone <$> xs)

    formatDone (_t, Left err)  = resultError (Text.pack err)
    formatDone (t, Right _ref) = formatSuccessComplete t

    formatSuccessComplete tx
      = resultSuccess ["completing task: " <> H.description tx]
