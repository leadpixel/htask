{-# LANGUAGE DeriveGeneric #-}

module HTask.CLI.Output.Document
  ( RunResult (..)
  , resultError
  , resultSuccess
  ) where

import           Data.Text    (Text)
import           GHC.Generics


data RunResult
  = RunResult
    { success :: Bool
    , text    :: [Text]
    }
  deriving (Generic, Show)


resultError :: Text -> RunResult
resultError x = RunResult
  { success = False
  , text = [x]
  }


resultSuccess :: [Text] -> RunResult
resultSuccess x = RunResult
  { success = True
  , text = x
  }
