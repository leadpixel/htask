module HTask.CLI.Actions.Complete
  where

import           HTask.CLI.Actions
import           Options.Applicative


completeInfo :: ParserInfo Action
completeInfo = info completeParser (progDesc "Mark a task as completed")


completeParser :: Parser Action
completeParser = Complete <$> argument str (metavar "TASKREF")
