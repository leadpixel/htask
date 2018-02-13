module HTask.CLI.Remove
  where

import HTask.Actions
import Options.Applicative


removeInfo :: ParserInfo Action
removeInfo = info removeParser (progDesc "remove things")


removeParser :: Parser Action
removeParser = Remove <$> argument str (metavar "TASKREF")
