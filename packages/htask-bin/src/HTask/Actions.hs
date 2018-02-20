module HTask.Actions
  where

import qualified Data.Text as Text


type TaskReference = Text.Text

type ShowUUID = Bool
type IncludeDeleted = Bool


data Action
  = Summary
  | List ShowUUID IncludeDeleted

  | Add Text.Text
  | Start TaskReference
  | Stop TaskReference
  | Complete TaskReference
  | Remove TaskReference

  | Pick
  | Drop
  | Done

  deriving (Show, Read)

