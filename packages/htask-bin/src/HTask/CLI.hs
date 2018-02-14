module HTask.CLI
  ( Options (..)
  , getOptions
  ) where

import Options.Applicative as Opts
import Data.Semigroup ((<>))

import HTask.Actions
import HTask.CLI.Summary
import HTask.CLI.List
import HTask.CLI.Add
import HTask.CLI.Start
import HTask.CLI.Complete
import HTask.CLI.Remove


data Options = Options
  { taskfile :: FilePath
  , action :: Action
  }


defaultAction :: Parser Action
defaultAction = pure Summary


actionParser :: Parser Action
actionParser = hsubparser
  (  command "summary"  summaryInfo
  <> command "list"     listInfo
  <> command "add"      addInfo
  <> command "start"    startInfo
  <> command "complete" completeInfo
  <> command "remove"   removeInfo
  <> command "ls"       listInfo
  )


fileParser :: Parser FilePath
fileParser = option str
  (  long "file"
  <> short 'f'
  <> showDefault
  <> help "path to tasks file"
  <> value "tasks.txt"
  )


optionsParser :: Parser Options
optionsParser = Options <$> fileParser <*> (actionParser <|> defaultAction)


optionsInfo :: ParserInfo Options
optionsInfo
  = info (helper <*> optionsParser)
  $ header "HTask"
  <> progDesc "track tasks in a local event log"


getOptions :: IO Options
getOptions = execParser optionsInfo
