module HTask.CLI.Pick
  where

import HTask.Actions
import Options.Applicative


pickInfo :: ParserInfo Action
pickInfo = info pickParser (progDesc "pick things")


pickParser :: Parser Action
pickParser = pure Pick
