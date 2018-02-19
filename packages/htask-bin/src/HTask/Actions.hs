module HTask.Actions
  where

import qualified Data.Text as Text


type TaskReference = Text.Text

data DetailFlag = ShowDetail | HideDetail
  deriving (Show, Read)


data Action
  = Summary
  | List DetailFlag

  | Add Text.Text
  | Start TaskReference
  | Stop TaskReference
  | Complete TaskReference
  | Remove TaskReference

  | Pick
  | Drop
  | Done

  deriving (Show, Read)

