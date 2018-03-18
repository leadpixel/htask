module HTask.CLI.Pick
  where

import HTask.Actions
import Options.Applicative


pickInfo :: ParserInfo Action
pickInfo = info pickParser (progDesc "Picks the next task by priority")


pickParser :: Parser Action
pickParser = pure Pick
