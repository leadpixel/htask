{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners
  ( runCommand
  ) where

import Data.Semigroup ((<>))
import Data.Tagged
import HTask.Actions
import HTask.CLI
import HTask.Runners.Summary
import HTask.Runners.List
import HTask.Runners.Pick
import HTask.Runners.Done
import HTask.TaskApplication
import qualified Control.Monad.Reader as R
import qualified Data.Text              as Text
import qualified Data.UUID as UUID
import qualified HTask as H


runCommand :: Options -> IO ()
runCommand opts = R.runReaderT (runAction $ action opts) (taskfile opts)


runAction :: Action -> TaskConfig ()
runAction Summary        = runSummary
runAction (List d)       = runList d
runAction (Add tex)      = runAdd tex
runAction (Start ref)    = runStart ref
runAction (Complete ref) = runComplete ref
runAction (Remove ref)   = runRemove ref
runAction Pick           = runPick
runAction Done           = runDone


justOne :: [a] -> Maybe a
justOne [x] = Just x
justOne _ = Nothing


findMatchingUUIDs :: Text.Text -> [H.Task] -> [Text.Text]
findMatchingUUIDs ref ts = filter (ref `Text.isPrefixOf`) (fmap taskRefToText ts)
  where
    taskRefToText :: H.Task -> Text.Text
    taskRefToText = UUID.toText . untag . H.taskRef


runAdd :: Text.Text -> TaskConfig ()
runAdd tex = runTask (H.addTask tex) >>= R.lift . putStrLn . show


runWithMatch :: (Show a) => (H.TaskRef -> TaskApplication a) -> Text.Text -> TaskConfig ()
runWithMatch f ref
  = do
    ts <- runTask H.listTasks
    output <- maybe
      (pure $ "did not find unique match for: " <> ref)
      (\v -> (Text.pack . show) <$> runTask (f $ Tagged v))
      (justOne (findMatchingUUIDs ref ts) >>= UUID.fromText)
    R.lift $ putStrLn (Text.unpack output)


runStart :: Text.Text -> TaskConfig ()
runStart = runWithMatch H.startTask


runComplete :: Text.Text -> TaskConfig ()
runComplete = runWithMatch H.completeTask


runRemove :: Text.Text -> TaskConfig ()
runRemove = runWithMatch H.deleteTask
