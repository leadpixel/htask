module HTask.CLI
  ( getOptions
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative as Opts

import           HTask.Actions
import           HTask.CLI.Add
import           HTask.CLI.Complete
import           HTask.CLI.Done
import           HTask.CLI.Drop
import           HTask.CLI.List
import           HTask.CLI.Pick
import           HTask.CLI.Remove
import           HTask.CLI.Start
import           HTask.CLI.Stop
import           HTask.CLI.Summary
import           HTask.Config


defaultAction :: Parser Action
defaultAction = pure Summary


actionParser :: Parser Action
actionParser = infoCommands <|> taskCommands <|> shortcutCommands
  where
    infoCommands
      = hsubparser
      $ commandGroup "Info"
        <> command "summary"  summaryInfo
        <> command "list"     listInfo

    taskCommands
      = hsubparser
      $ commandGroup "Tasks"
        <> command "add"      addInfo
        <> command "start"    startInfo
        <> command "stop"     stopInfo
        <> command "complete" completeInfo
        <> command "remove"   removeInfo
        <> hidden

    shortcutCommands
      = hsubparser
      $ commandGroup "Shortcuts"
        <> command "ls"       listInfo
        <> command "pick"     pickInfo
        <> command "drop"     dropInfo
        <> command "done"     doneInfo
        <> hidden


fileParser :: Parser FilePath
fileParser = option str
  (  long "file"
  <> short 'f'
  <> showDefault
  <> help "path to tasks file"
  <> value ".tasks"
  )



optionsParser :: Parser Options
optionsParser
  = flip Options
  <$> fileParser
  <*> (actionParser <|> defaultAction)


optionsInfo :: ParserInfo Options
optionsInfo
  = info (helper <*> optionsParser)
  $ header "HTask"
  <> progDesc "track tasks in a local event log"


getOptions :: IO Options
getOptions = execParser optionsInfo
