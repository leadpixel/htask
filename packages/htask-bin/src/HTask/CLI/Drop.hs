module HTask.CLI.Drop
  where

import HTask.Actions
import Options.Applicative


dropInfo :: ParserInfo Action
dropInfo = info dropParser (progDesc "Drops the current tasks in progress")


dropParser :: Parser Action
dropParser = pure Drop
