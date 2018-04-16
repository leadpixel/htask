{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Complete
  ( runComplete
  ) where

import Control.Monad.IO.Class
import Event
import qualified Data.Text as Text
import qualified HTask as H
import HTask.Runners.Common
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))


type CompleteOutput = Either String H.TaskRef


runComplete :: (H.HasTasks (TaskApplication m), H.CanCreateTask m, MonadIO m) => Text.Text -> EventBackend m Document
runComplete
  = withMatch $ \tx ->
      presentComplete tx <$> executeComplete tx


executeComplete :: (H.HasTasks (TaskApplication m), H.CanCreateTask m, MonadIO m) => H.Task -> EventBackend m CompleteOutput
executeComplete
  = runTask . H.completeTask . H.taskRef


presentComplete :: H.Task -> CompleteOutput -> Document
presentComplete tx
  = either
      (formatError . Text.pack)
      (const $ formatSuccessComplete tx)

  where
    formatSuccessComplete tx'
      = formatSuccess ("completing task: " <> H.description tx')
