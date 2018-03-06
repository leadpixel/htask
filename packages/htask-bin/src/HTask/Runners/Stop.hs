module HTask.Runners.Stop
  ( runStop
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output


runStop :: Text.Text -> TaskConfig Document
runStop = runWithMatch H.stopTask
