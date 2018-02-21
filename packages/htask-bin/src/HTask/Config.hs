  module HTask.Config
  ( Options (..)
  , GlobalOptions (..)
  , Formatter (..)
  ) where

import HTask.Actions


data Formatter
  = Default
  | JSON
  | Porcelain
  deriving (Show, Read)


data GlobalOptions = GlobalOptions
  { taskfile :: FilePath
  , formatter :: Formatter
  }


data Options = Options
  { globals :: GlobalOptions
  , action :: Action
  }

