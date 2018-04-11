module HTask.Output.Document
  ( Document (..)
  , Block (..)
  , line
  ) where

import qualified Data.Text as Text


newtype Document = Document { undoc :: [Block] }

instance Monoid Document where
  mempty = Document []
  mappend (Document a) (Document b) = Document (mappend a b)


newtype Block
  = Line Text.Text


line :: Text.Text -> Block
line = Line

