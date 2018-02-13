module HTask.CLI.Add
  where

import HTask.Actions
import Options.Applicative


addInfo :: ParserInfo Action
addInfo = info addParser (progDesc "add things")


addParser :: Parser Action
addParser = Add <$> argument str (metavar "DESCRIPTION")
