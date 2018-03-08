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


formatAdd :: Text.Text -> Either String a -> Document
formatAdd t x
  = case x of
      Left e -> formatError e
      Right _ -> formatSuccessAdd t


formatSuccessAdd :: Text.Text -> Document
formatSuccessAdd t
  = formatSuccess ("added task: " <> t)
