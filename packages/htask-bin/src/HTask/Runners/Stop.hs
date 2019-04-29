{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Stop
  ( runStop
  ) where

import           Data.Semigroup        ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified HTask                 as H
import           HTask.Output
import           HTask.Runners.Common
import           HTask.TaskApplication


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
