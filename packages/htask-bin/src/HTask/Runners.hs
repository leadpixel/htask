{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners
  ( runCommand
  ) where

import Data.Semigroup ((<>))
import Data.Tagged
import HTask.Actions
import HTask.Config
import HTask.Output
import HTask.Runners.Summary
import HTask.Runners.List
import HTask.Runners.Pick
import HTask.Runners.Done
import HTask.Runners.Drop
import HTask.TaskApplication
import qualified Control.Monad.Reader as R
import qualified Data.Text              as Text
import qualified Data.UUID as UUID
import qualified HTask as H


runCommand :: Options -> IO ()
runCommand opts
  = R.runReaderT (runAction $ action opts) (globals opts)
  >>= renderOutput (formatter $ globals opts)


runAction :: Action -> TaskConfig Output
runAction Summary        = runSummary
runAction (List d k)     = runList d k
runAction (Add tex)      = runAdd tex
runAction (Start ref)    = runStart ref
runAction (Stop ref)     = runStop ref
runAction (Complete ref) = runComplete ref
runAction (Remove ref)   = runRemove ref
runAction Pick           = runPick
runAction Drop           = runDrop
runAction Done           = runDone


justOne :: [a] -> Maybe a
justOne [x] = Just x
justOne _ = Nothing


findMatchingUUIDs :: Text.Text -> [H.Task] -> [Text.Text]
findMatchingUUIDs ref ts = filter (ref `Text.isPrefixOf`) (fmap taskRefToText ts)
  where
    taskRefToText :: H.Task -> Text.Text
    taskRefToText = UUID.toText . untag . H.taskRef


runAdd :: Text.Text -> TaskConfig Output
runAdd tex
  = runTask (H.addTask tex)
  >>= \x -> pure [ line $ Text.pack (show x) ]


runWithMatch :: (Show a) => (H.TaskRef -> TaskApplication a) -> Text.Text -> TaskConfig Output
runWithMatch f ref
  = do
    ts <- runTask H.listTasks
    output <- maybe
      (pure $ "did not find unique match for: " <> ref)
      (\v -> (Text.pack . show) <$> runTask (f $ Tagged v))
      (justOne (findMatchingUUIDs ref ts) >>= UUID.fromText)
    pure [ line output ]


runStart :: Text.Text -> TaskConfig Output
runStart = runWithMatch H.startTask


runStop :: Text.Text -> TaskConfig Output
runStop = runWithMatch H.stopTask


runComplete :: Text.Text -> TaskConfig Output
runComplete = runWithMatch H.completeTask


runRemove :: Text.Text -> TaskConfig Output
runRemove = runWithMatch H.deleteTask
