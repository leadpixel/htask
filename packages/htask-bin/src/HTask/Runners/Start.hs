{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Start
  ( runStart
  ) where

import qualified Data.Text             as Text
import qualified HTask.API             as API
import qualified HTask.Task            as H

import           HTask.Output.Document
import           HTask.Runners.Common
import           HTask.TaskApplication

import           Data.Semigroup        ((<>))
import           Data.Text             (Text)


runStart :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runStart = withMatch
  (\tx -> runTask
    $   formatOutcome tx
    <$> API.startTask (H.taskRef tx)
  )

  where
    formatOutcome tx
      = either
          (resultError . Text.pack)
          (const $ formatSuccessStart tx)

    formatSuccessStart tx
      = resultSuccess ["starting task: " <> H.description tx]
