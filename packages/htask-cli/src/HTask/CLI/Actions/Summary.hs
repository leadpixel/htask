module HTask.CLI.Actions.Summary where

import           HTask.CLI.Actions
import           Options.Applicative


summaryInfo :: ParserInfo Action
summaryInfo = info summaryParser (progDesc "A short summary of current tasks")


summaryParser :: Parser Action
summaryParser = pure Summary
