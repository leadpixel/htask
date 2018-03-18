module HTask.CLI.Remove
  where

import HTask.Actions
import Options.Applicative


removeInfo :: ParserInfo Action
removeInfo = info removeParser (progDesc "Removes a task from the list")


removeParser :: Parser Action
removeParser = Remove <$> argument str (metavar "TASKREF")
