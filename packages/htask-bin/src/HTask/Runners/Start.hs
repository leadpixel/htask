{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Start
  ( runStart
  ) where

import Control.Monad.IO.Class
import Event
import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))


runStart :: (HasEventBackend m, H.CanCreateTask m) => Text.Text -> m Document
runStart = withMatch
  (\tx -> runTask
    $   formatOutcome tx
    <$> H.startTask (H.taskRef tx)
  )

  where
    formatOutcome tx
      = either
          (formatError . Text.pack)
          (const $ formatSuccessStart tx)

    formatSuccessStart tx
      = formatSuccess ("starting task: " <> H.description tx)
