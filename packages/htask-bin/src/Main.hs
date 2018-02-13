module Main
  ( main
  ) where

import HTask.Runners
import HTask.CLI
import Data.Aeson
import Data.Maybe
import HTask.TaskApplication
import qualified Control.Monad.State    as State
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.List              as List
import qualified HTask as H


readTaskEvents :: FilePath -> IO [H.TaskEvent]
readTaskEvents p = (parseLines . lines) <$> readFile p
  where
    parseLines :: [String] -> [H.TaskEvent]
    parseLines = catMaybes . fmap (decode . UTF8.fromString)


prepTasks :: [H.TaskEvent] -> IO [H.Task]
prepTasks vs
  = State.execStateT
      (unwrapTaskApp $ H.replayEventLog vs)
      H.emptyTasks


main :: IO ()
main = do
  options <- getOptions

  vs <- readTaskEvents "tasks.txt"
  xs <- prepTasks vs
  ts <- runTask H.listTasks xs

  runCommand options (List.sortOn H.createdAt ts)
