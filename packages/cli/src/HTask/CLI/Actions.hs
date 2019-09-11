module HTask.CLI.Actions
  where

import           Data.Text (Text)


type ShowUUID = Bool
type ShowAll = Bool


data Action
  = Summary
  | List ShowUUID ShowAll

  | Add Text
  | Start Text
  | Stop Text
  | Complete Text
  | Remove Text

  | Pick
  | Drop
  | Done

  deriving (Show, Read)
