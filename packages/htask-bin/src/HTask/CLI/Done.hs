module HTask.CLI.Done
  where

import           HTask.Actions
import           Options.Applicative


doneInfo :: ParserInfo Action
doneInfo = info doneParser (progDesc "Marks the current task as completed")


doneParser :: Parser Action
doneParser = pure Done
