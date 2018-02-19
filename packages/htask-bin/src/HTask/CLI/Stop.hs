module HTask.CLI.Stop
  where

import HTask.Actions
import Options.Applicative


stopInfo :: ParserInfo Action
stopInfo = info stopParser (progDesc "Stops a task")


stopParser :: Parser Action
stopParser = Stop <$> argument str (metavar "TASKREF")
