module HTask.Config
  ( Options (..)
  ) where

import           HTask.Actions


data Options = Options
  { action   :: Action
  , taskfile :: FilePath
  }
