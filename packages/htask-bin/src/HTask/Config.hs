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


-- instance Show Formatter where
--   show Default = "default"
--   show JSON = "json"
--   show Porcelain = "porcelain"


-- instance Read Formatter where
--   readsPrec = undefined


data GlobalOptions = GlobalOptions
  { taskfile :: FilePath
  , formatter :: Formatter
  }


data Options = Options
  { globals :: GlobalOptions
  , action :: Action
  }

