module HTask.CLI.Actions.Start where

import           HTask.CLI.Actions
import           Options.Applicative


startInfo :: ParserInfo Action
startInfo = info startParser (progDesc "Starts a given task")


startParser :: Parser Action
startParser = Start <$> argument str (metavar "TASKREF")
