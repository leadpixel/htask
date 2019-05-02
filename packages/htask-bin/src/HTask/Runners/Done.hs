{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module HTask.Runners.Done
  ( runDone
  ) where

import qualified Data.Text             as Text
import qualified Event                 as V
import qualified HTask.API             as API
import qualified HTask.Task            as H
import qualified HTask.TaskContainer   as HC

import           HTask.Output.Document
import           HTask.TaskApplication

import           Data.Semigroup        ((<>))


type DoneOutput = [(H.Task, Either String H.TaskRef)]


runDone :: (HasEventBackend m, H.CanCreateTask m) => m RunResult
runDone
  = formatOutcome <$> runTask doneTask

  where

    doneTask :: (V.CanUuid m, V.CanTime m, Monad m, HC.HasTasks m, V.HasEventSink m) => m DoneOutput
    doneTask = do
      xs <- API.listTasks
      let ts = filter isCurrent xs
      mapM execDone ts

    execDone t = (t,) <$> API.completeTask (H.taskRef t)

    isCurrent t = H.status t == H.InProgress

    formatOutcome :: DoneOutput -> RunResult
    formatOutcome xs
      = undefined (formatDone <$> xs)

    formatDone (_t, Left err)  = resultError (Text.pack err)
    formatDone (t, Right _ref) = formatSuccessComplete t

    formatSuccessComplete tx
      = resultSuccess ["completing task: " <> H.description tx]
