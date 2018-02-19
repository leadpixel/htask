module HTask.CLI.Start
  where

import HTask.Actions
import Options.Applicative


startInfo :: ParserInfo Action
startInfo = info startParser (progDesc "Starts a task")


startParser :: Parser Action
startParser = Start <$> argument str (metavar "TASKREF")
