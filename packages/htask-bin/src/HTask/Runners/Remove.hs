module HTask.Runners.Remove
  ( runRemove
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output


runRemove :: Text.Text -> TaskConfig Document
runRemove = runWithMatch H.removeTask
