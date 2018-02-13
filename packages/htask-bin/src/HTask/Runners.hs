{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners
  ( runCommand
  ) where

import HTask.Runners.List
import qualified HTask as H
import HTask.Actions
import Control.Monad
import Data.Semigroup ((<>))
import Data.Tagged
import Data.List
import HTask.TaskApplication
import qualified Data.Text              as Text
import qualified Data.UUID as UUID


runCommand :: Action -> FilePath -> IO ()
runCommand (List d)       = runList d
runCommand (Add tex)      = runAdd tex
runCommand (Start ref)    = runStart ref
runCommand (Complete ref) = runComplete ref
runCommand (Remove ref)   = runRemove ref


justOne :: [a] -> Maybe a
justOne [x] = Just x
justOne _ = Nothing


findMatchingUUIDs :: Text.Text -> [H.Task] -> [Text.Text]
findMatchingUUIDs ref ts = filter (ref `Text.isPrefixOf`) (fmap taskRefToText ts)
  where
    taskRefToText :: H.Task -> Text.Text
    taskRefToText = UUID.toText . untag . H.taskRef


runAdd :: Text.Text -> FilePath -> IO ()
runAdd tex file = runTask (H.addTask tex) file >>= print


runWithMatch :: (Show a) => (H.TaskRef -> TaskApplication a) -> Text.Text -> FilePath -> IO ()
runWithMatch f ref file
  = do
    ts <- runTask H.listTasks file
    maybe
      (print $ "did not find unique match for: " <> ref)
      (\v -> runTask (f $ Tagged v) file >>= print)
      (justOne (findMatchingUUIDs ref ts) >>= UUID.fromText)


runStart :: Text.Text -> FilePath -> IO ()
runStart = runWithMatch H.startTask


runComplete :: Text.Text -> FilePath -> IO ()
runComplete = runWithMatch H.completeTask


runRemove :: Text.Text -> FilePath -> IO ()
runRemove = runWithMatch H.deleteTask
