module HTask.Output.Document
  ( Document (..)
  , Block (..)
  , line
  ) where

import qualified Data.Text as Text


newtype Document = Document { undoc :: [Block] }


newtype Block
  = Line Text.Text


line :: Text.Text -> Block
line = Line

