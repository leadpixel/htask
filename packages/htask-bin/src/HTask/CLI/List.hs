module HTask.CLI.List
  where

import HTask.Actions
import Options.Applicative


listInfo :: ParserInfo Action
listInfo = info listParser (progDesc "list one")


listParser :: Parser Action
listParser = pure List
