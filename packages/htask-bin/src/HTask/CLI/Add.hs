module HTask.CLI.Add
  where

import HTask.Actions
import Options.Applicative
import Data.Text as Text


addInfo :: ParserInfo Action
addInfo = info addParser (progDesc "add things")


addParser :: Parser Action
addParser = Add <$> argument k (metavar "DESCRIPTION")
  where
    k :: ReadM Text
    k = str
