{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Start
  ( runStart
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))


runStart :: Text.Text -> TaskConfig IO Document
runStart = withMatch
  (\tx -> runTask
    $   formatOutcome tx
    <$> H.startTask (H.taskRef tx)
  )

  where
    formatOutcome tx
      = either
          (formatError . Text.pack)
          (const $ formatSuccessStart tx)

    formatSuccessStart tx
      = formatSuccess ("starting task: " <> H.description tx)
