module HTask.CLI.Summary
  where

import HTask.Actions
import Options.Applicative


summaryInfo :: ParserInfo Action
summaryInfo = info summaryParser (progDesc "A short summary of current tasks")


summaryParser :: Parser Action
summaryParser = pure Summary
