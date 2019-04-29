{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Stop
  ( runStop
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))
import Data.Text (Text)


runStop :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runStop = withMatch
  (\tx -> runTask
    $   formatOutcome tx
    <$> H.stopTask (H.taskRef tx)
  )

  where
    formatOutcome tx
      = either
          (resultError . Text.pack)
          (const $ formatSuccessStop tx)

    formatSuccessStop tx
      = resultSuccess [("stopping task: " <> H.description tx)]
