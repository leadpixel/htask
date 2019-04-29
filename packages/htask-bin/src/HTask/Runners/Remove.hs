{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Remove
  ( runRemove
  ) where

import           Data.Semigroup        ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified HTask                 as H
import           HTask.Output
import           HTask.Runners.Common
import           HTask.TaskApplication


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
