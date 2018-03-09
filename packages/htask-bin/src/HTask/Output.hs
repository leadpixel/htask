{-# LANGUAGE OverloadedStrings #-}

module HTask.Output
  ( module HTask.Output.Document
  , module HTask.Output.Formatters
  , module HTask.Output.Renderers

  , formatError
  , formatSuccess
  ) where

import qualified Data.Text as Text
import HTask.Output.Formatters
import HTask.Output.Document
import HTask.Output.Renderers
import Data.Semigroup ((<>))


formatError :: Text.Text -> Document
formatError t = Document
  [ line (withColor Red "Error" <> ":")
  , line t
  ]


formatSuccess :: Text.Text -> Document
formatSuccess t
  = Document
    [ line (withColor Green "Success!")
    , line t
    ]
