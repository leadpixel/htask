{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Add
  ( runAdd
  ) where

import Control.Monad.IO.Class
import Event
import qualified Data.Text as Text
import qualified HTask as H
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))


type AddOutput = Either String H.TaskRef


runAdd :: (MonadIO m, H.CanCreateTask m, H.HasTasks (TaskApplication m)) => Text.Text -> EventBackend m Document
runAdd t
  = presentAdd t <$> executeAdd t


executeAdd :: (MonadIO m, H.CanCreateTask m, H.HasTasks (TaskApplication m)) => Text.Text -> EventBackend m AddOutput
executeAdd
  = runTask . H.addTask


presentAdd :: Text.Text -> AddOutput -> Document
presentAdd t
  = either
      (formatError . Text.pack)
      (formatSuccessAdd t)

  where
    formatSuccessAdd t' ref
      = formatSuccess
          (  "added task: " <> t' <> "\n"
          <> "ref: " <> H.taskRefText ref
          )
