{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Remove
  ( runRemove
  ) where

import qualified Data.Text             as Text
import qualified HTask.API             as API
import qualified HTask.Task            as H

import           HTask.Output.Document
import           HTask.Runners.Common
import           HTask.TaskApplication

import           Data.Semigroup        ((<>))
import           Data.Text             (Text)


runRemove :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runRemove = withMatch
  (\tx -> runTask
    $   formatOutcome tx
    <$> API.removeTask (H.taskRef tx)
  )

  where
    formatOutcome tx
      = either
          (resultError . Text.pack)
          (const $ formatSuccessRemove tx)

    formatSuccessRemove tx
      = resultSuccess [ "removing task: " <> H.description tx]
