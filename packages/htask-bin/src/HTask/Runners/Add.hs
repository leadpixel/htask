{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Add
  ( runAdd
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))


runAdd :: Text.Text -> TaskConfig Document
runAdd t
  = formatOutcome t <$> runTask (H.addTask t)

  where
    formatOutcome t'
      = either
          (formatError . Text.pack)
          (formatSuccessAdd t')

    formatSuccessAdd t' ref
      = formatSuccess
          (  "added task: " <> t' <> "\n"
          <> "ref: " <> H.taskRefText ref
          )
