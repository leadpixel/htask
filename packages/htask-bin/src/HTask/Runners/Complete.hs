{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Complete
  ( runComplete
  ) where

import           Data.Semigroup        ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified HTask                 as H
import           HTask.Output
import           HTask.Runners.Common
import           HTask.TaskApplication


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
