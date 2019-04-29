module HTask.Actions
  where

import Data.Text (Text)


type TaskReference = Text

type ShowUUID = Bool
type ShowAll = Bool


data Action
  = Summary
  | List ShowUUID ShowAll

  | Add Text
  | Start TaskReference
  | Stop TaskReference
  | Complete TaskReference
  | Remove TaskReference

  | Pick
  | Drop
  | Done

  deriving (Show, Read)

