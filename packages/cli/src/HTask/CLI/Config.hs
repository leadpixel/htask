module HTask.CLI.Config
  ( Options (..)
  ) where

import           HTask.CLI.Actions


data Options = Options
  { action   :: Action
  , taskfile :: FilePath
  }
