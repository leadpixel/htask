{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Complete
  ( runComplete
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))
import Data.Text (Text)


type CompleteOutput = Either String H.TaskRef


runComplete :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runComplete
  = withMatch $ \tx ->
      presentComplete tx <$> executeComplete tx


executeComplete :: (HasEventBackend m, H.CanCreateTask m) => H.Task -> m CompleteOutput
executeComplete
  = runTask . H.completeTask . H.taskRef


presentComplete :: H.Task -> CompleteOutput -> RunResult
presentComplete tx
  = either
      (resultError . Text.pack)
      (const $ formatSuccessComplete tx)

  where
    formatSuccessComplete tx'
      = resultSuccess [ ("completing task: " <> H.description tx')]
