module HTask.CLI.Options
  ( Options (..)
  , getOptions
  ) where

import qualified Options.Applicative        as Opts

import           HTask.CLI.Actions          (Action)
import           Options.Applicative        ((<|>))

import           HTask.CLI.Actions.Add
import           HTask.CLI.Actions.Complete
import           HTask.CLI.Actions.Done
import           HTask.CLI.Actions.Drop
import           HTask.CLI.Actions.List
import           HTask.CLI.Actions.Pick
import           HTask.CLI.Actions.Remove
import           HTask.CLI.Actions.Start
import           HTask.CLI.Actions.Stop
import           HTask.CLI.Actions.Summary


data Options = Options
  { action   :: Action
  , taskfile :: FilePath
  }


actionParser :: Opts.Parser Action
actionParser = infoCommands <|> taskCommands <|> shortcutCommands
  where
    infoCommands
      = Opts.hsubparser
      $ Opts.commandGroup "Info"
        <> Opts.command "summary"  summaryInfo
        <> Opts.command "list"     listInfo

    taskCommands
      = Opts.hsubparser
      $ Opts.commandGroup "Tasks"
        <> Opts.command "add"      addInfo
        <> Opts.command "start"    startInfo
        <> Opts.command "stop"     stopInfo
        <> Opts.command "complete" completeInfo
        <> Opts.command "remove"   removeInfo
        <> Opts.hidden

    shortcutCommands
      = Opts.hsubparser
      $ Opts.commandGroup "Shortcuts"
        <> Opts.command "ls"       listInfo
        <> Opts.command "pick"     pickInfo
        <> Opts.command "drop"     dropInfo
        <> Opts.command "done"     doneInfo
        <> Opts.hidden


fileParser :: Opts.Parser FilePath
fileParser = Opts.option Opts.str
  ( Opts.long "file"
  <> Opts.short 'f'
  <> Opts.showDefault
  <> Opts.help "path to tasks file"
  <> Opts.value "~/.tasks"
  )


optionsParser :: Opts.Parser Options
optionsParser
  = Options
  <$> (actionParser <|> defaultAction)
  <*> fileParser

  where
    defaultAction :: Opts.Parser Action
    defaultAction = summaryParser


optionsInfo :: Opts.ParserInfo Options
optionsInfo
  = Opts.info (Opts.helper <*> optionsParser)
  $ Opts.header "HTask.CLI"
  <> Opts.progDesc "track tasks in a local event log"


getOptions :: IO Options
getOptions = Opts.execParser optionsInfo
