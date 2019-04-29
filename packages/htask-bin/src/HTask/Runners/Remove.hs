{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Remove
  ( runRemove
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))
import Data.Text (Text)


runRemove :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runRemove = withMatch
  (\tx -> runTask
    $   formatOutcome tx
    <$> H.removeTask (H.taskRef tx)
  )

  where
    formatOutcome tx
      = either
          (resultError . Text.pack)
          (const $ formatSuccessRemove tx)

    formatSuccessRemove tx
      = resultSuccess [ ("removing task: " <> H.description tx)]
