module HTask.CLI.Add
  where

import           HTask.Actions
import           Options.Applicative


addInfo :: ParserInfo Action
addInfo = info addParser (progDesc "Add a task description")


addParser :: Parser Action
addParser = Add <$> argument str (metavar "DESCRIPTION")
