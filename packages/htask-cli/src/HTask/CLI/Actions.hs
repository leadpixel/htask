module HTask.CLI.Actions
  ( Action (..)
  , ShowAll
  , ShowUUID
  ) where

import           Data.Tagged
import           Data.Text   (Text)


data ShowUUIDTag
type ShowUUID = Tagged ShowUUIDTag Bool


data ShowAllTag
type ShowAll = Tagged ShowAllTag Bool


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
  deriving (Read, Show)
