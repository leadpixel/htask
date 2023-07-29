module HTask.CLI.Actions.Stop
  where

import           HTask.CLI.Actions
import           Options.Applicative


stopInfo :: ParserInfo Action
stopInfo = info stopParser (progDesc "Stops a given task")


stopParser :: Parser Action
stopParser = Stop <$> argument str (metavar "TASKREF")
