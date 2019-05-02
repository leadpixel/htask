{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Stop
  ( runStop
  ) where

import qualified Data.Text             as Text
import qualified HTask.API             as API
import qualified HTask.Task            as H

import           HTask.Output.Document
import           HTask.Runners.Common
import           HTask.TaskApplication

import           Data.Semigroup        ((<>))
import           Data.Text             (Text)


runStop :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runStop = withMatch
  (\tx -> runTask
    $   formatOutcome tx
    <$> API.stopTask (H.taskRef tx)
  )

  where
    formatOutcome tx
      = either
          (resultError . Text.pack)
          (const $ formatSuccessStop tx)

    formatSuccessStop tx
      = resultSuccess ["stopping task: " <> H.description tx]
