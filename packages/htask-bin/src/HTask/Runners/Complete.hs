module HTask.Runners.Complete
  ( runComplete
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output


runComplete :: Text.Text -> TaskConfig Document
runComplete = runWithMatch H.completeTask
