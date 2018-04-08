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


runAdd :: Text.Text -> TaskConfig Document
runAdd t
  = presentAdd t <$> executeAdd t


executeAdd :: Text.Text -> TaskConfig AddOutput
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
