module HTask.Runners.Start
  ( runStart
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output


runStart :: Text.Text -> TaskConfig Document
runStart = runWithMatch H.startTask
