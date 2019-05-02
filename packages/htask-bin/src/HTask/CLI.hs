module HTask.CLI
  ( Options (..)
  , getOptions
  ) where

import qualified Options.Applicative as Opts

import           Data.Semigroup      ((<>))
import           HTask.Actions       (Action)
import           HTask.Config        (Options (..))
import           Options.Applicative ((<|>))

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
  <> Opts.value ".tasks"
  )


optionsParser :: Opts.Parser Options
optionsParser
  = flip Options
  <$> fileParser
  <*> (actionParser <|> defaultAction)

  where
    defaultAction :: Opts.Parser Action
    defaultAction = summaryParser


optionsInfo :: Opts.ParserInfo Options
optionsInfo
  = Opts.info (Opts.helper <*> optionsParser)
  $ Opts.header "HTask"
  <> Opts.progDesc "track tasks in a local event log"


getOptions :: IO Options
getOptions = Opts.execParser optionsInfo
