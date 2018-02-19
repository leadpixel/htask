module HTask.CLI.Drop
  where

import HTask.Actions
import Options.Applicative


dropInfo :: ParserInfo Action
dropInfo = info dropParser (progDesc "Drops the current in progress tasks")


dropParser :: Parser Action
dropParser = pure Drop
