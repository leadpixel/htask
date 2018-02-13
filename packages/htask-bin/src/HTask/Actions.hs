module HTask.Actions
  where

import qualified Data.Text as Text


data Action
  = List
  | Add Text.Text
  | Start Text.Text
  | Remove Text.Text
  deriving (Show, Read)

