{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Remove
  ( runRemove
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))


runRemove :: Text.Text -> TaskConfig Document
runRemove = withMatch
  (\tx -> runTask
    $   formatOutcome tx
    <$> H.removeTask (H.taskRef tx)
  )

  where
    formatOutcome tx
      = either
          (formatError . Text.pack)
          (const $ formatSuccessRemove tx)

    formatSuccessRemove tx
      = formatSuccess ("removing task: " <> H.description tx)
