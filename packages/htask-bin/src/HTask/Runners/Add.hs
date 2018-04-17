{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Add
  ( runAdd
  ) where

import qualified Data.Text as Text
import qualified HTask as H

import HTask.TaskApplication
import HTask.Output

import Data.Semigroup ((<>))


type AddOutput = Either String H.TaskRef


runAdd :: (HasEventBackend m, H.CanCreateTask m) => Text.Text -> m Document
runAdd t
  = presentAdd t <$> runTask (H.addTask t)


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
