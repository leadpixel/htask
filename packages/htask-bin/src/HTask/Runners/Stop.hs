{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Stop
  ( runStop
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))


runStop :: Text.Text -> TaskConfig Document
runStop = withMatch
  (\tx -> runTask
    $   formatOutcome tx
    <$> H.stopTask (H.taskRef tx)
  )

  where
    formatOutcome tx
      = either
          (formatError . Text.pack)
          (const $ formatSuccessStop tx)

    formatSuccessStop tx
      = formatSuccess ("stopping task: " <> H.description tx)
