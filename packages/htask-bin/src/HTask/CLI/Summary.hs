module HTask.CLI.Summary
  where

import HTask.Actions
import Options.Applicative


summaryInfo :: ParserInfo Action
summaryInfo = info summaryParser (progDesc "Summary")


summaryParser :: Parser Action
summaryParser = pure Summary
