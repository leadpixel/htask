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
runAdd tex
  = formatAdd tex <$> runTask (H.addTask tex)


formatAdd :: Text.Text -> Either String H.TaskRef -> Document
formatAdd t x
  = case x of
      Left e -> formatError (Text.pack e)
      Right v -> formatSuccessAdd t v


formatSuccessAdd :: Text.Text -> H.TaskRef -> Document
formatSuccessAdd t v
  = formatSuccess ("added task: " <> t <> "\nref: " <> H.taskRefText v)
