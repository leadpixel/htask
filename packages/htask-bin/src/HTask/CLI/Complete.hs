module HTask.CLI.Complete
  where

import           HTask.Actions
import           Options.Applicative


completeInfo :: ParserInfo Action
completeInfo = info completeParser (progDesc "Mark a task as completed")


completeParser :: Parser Action
completeParser = Complete <$> argument str (metavar "TASKREF")
