{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Complete
  ( runComplete
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))


runComplete :: Text.Text -> TaskConfig Document
runComplete = withMatch
  (\tx -> runTask
    $   formatOutcome tx
    <$> H.completeTask (H.taskRef tx)
  )

  where
    formatOutcome tx
      = either
          (formatError . Text.pack)
          (const $ formatSuccessComplete tx)

    formatSuccessComplete tx
      = formatSuccess ("completing task: " <> H.description tx)
