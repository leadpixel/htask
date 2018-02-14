module HTask.Actions
  where

import qualified Data.Text as Text


data DetailFlag = ShowDetail | HideDetail
  deriving (Show, Read)


data Action
  = Summary
  | List DetailFlag
  | Add Text.Text
  | Start Text.Text
  | Complete Text.Text
  | Remove Text.Text
  | Pick
  | Done
  deriving (Show, Read)

