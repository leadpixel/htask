{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Start
  ( runStart
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))
import Data.Text (Text)


runStart :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runStart = withMatch
  (\tx -> runTask
    $   formatOutcome tx
    <$> H.startTask (H.taskRef tx)
  )

  where
    formatOutcome tx
      = either
          (resultError . Text.pack)
          (const $ formatSuccessStart tx)

    formatSuccessStart tx
      = resultSuccess [("starting task: " <> H.description tx)]
