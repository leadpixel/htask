module HTask.CLI
  ( getOptions
  ) where

import Options.Applicative as Opts
import Data.Semigroup ((<>))

import HTask.Actions
import HTask.Config
import HTask.CLI.Add
import HTask.CLI.Complete
import HTask.CLI.Done
import HTask.CLI.Drop
import HTask.CLI.List
import HTask.CLI.Pick
import HTask.CLI.Remove
import HTask.CLI.Start
import HTask.CLI.Stop
import HTask.CLI.Summary


defaultAction :: Parser Action
defaultAction = pure Summary


actionParser :: Parser Action
actionParser = hsubparser
  (  command "summary"  summaryInfo
  <> command "list"     listInfo
  <> command "add"      addInfo
  <> command "start"    startInfo
  <> command "stop"     stopInfo
  <> command "complete" completeInfo
  <> command "remove"   removeInfo

  <> command "pick"     pickInfo
  <> command "drop"     dropInfo
  <> command "done"     doneInfo

  <> command "ls"       listInfo
  )


fileParser :: Parser FilePath
fileParser = option str
  (  long "file"
  <> short 'f'
  <> showDefault
  <> help "path to tasks file"
  <> value ".tasks"
  )


formatterParser :: Parser Formatter
formatterParser = option auto
  (  long "format"
  <> short 'o'
  <> showDefault
  <> help "Select an output format"
  <> value Default
  )


globalsParser :: Parser GlobalOptions
globalsParser = GlobalOptions <$> fileParser <*> formatterParser


optionsParser :: Parser Options
optionsParser = Options <$> globalsParser <*> (actionParser <|> defaultAction)


optionsInfo :: ParserInfo Options
optionsInfo
  = info (helper <*> optionsParser)
  $ header "HTask"
  <> progDesc "track tasks in a local event log"


getOptions :: IO Options
getOptions = execParser optionsInfo
