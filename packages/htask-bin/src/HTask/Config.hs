module HTask.Config
  ( Options (..)
  , Formatter (..)
  ) where

import HTask.Actions


data Formatter
  = Terminal
  | JSON
  | Porcelain
  deriving (Show, Read)


data Options = Options
  { action :: Action
  , taskfile :: FilePath
  , formatter :: Formatter
  }
