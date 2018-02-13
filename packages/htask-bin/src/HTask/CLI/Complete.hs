module HTask.CLI.Complete
  where

import HTask.Actions
import Options.Applicative


completeInfo :: ParserInfo Action
completeInfo = info completeParser (progDesc "complete things")


completeParser :: Parser Action
completeParser = Complete <$> argument str (metavar "TASKREF")
