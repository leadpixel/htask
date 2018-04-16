{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Remove
  ( runRemove
  ) where

import Control.Monad.IO.Class
import Event
import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))


runRemove :: (H.HasTasks (TaskApplication m), H.CanCreateTask m, MonadIO m) => Text.Text -> EventBackend m Document
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
