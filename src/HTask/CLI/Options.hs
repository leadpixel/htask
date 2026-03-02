{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTask.CLI.Options
  ( Options (..)
  , getOptions
  ) where

import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import           Data.Tagged         (Tagged (..))
import qualified Data.Text           as Text
import           Data.Version        (showVersion)
import           HTask.CLI.Actions   (Action (..))
import qualified HTask.Core          as H
import qualified HTask.Events        as V
import           Options.Applicative
import           Paths_htask         (version)
import           System.Directory    (doesFileExist, getHomeDirectory)
import           System.FilePath     ((</>))


data Options
  = Options
    { action   :: Action
    , taskfile :: FilePath
    }
  deriving (Show)

data RawOptions
  = RawOptions
    { rawAction :: Action
    , rawFile   :: Maybe FilePath
    }

optionsParser :: Parser RawOptions
optionsParser
  = RawOptions
  <$> (hsubparser
      (  command "add"      addInfo
      <> command "complete" completeInfo
      <> command "done"     doneInfo
      <> command "drop"     dropInfo
      <> command "list"     listInfo
      <> command "pick"     pickInfo
      <> command "remove"   removeInfo
      <> command "start"    startInfo
      <> command "stop"     stopInfo
      <> command "summary"  summaryInfo
      ) <|> pure Summary)
  <*> optional (strOption
      (  long "file"
      <> short 'f'
      <> metavar "ARG"
      <> help "path to the task list file (default: ~/.tasks)"
      ))

optionsInfo :: ParserInfo RawOptions
optionsInfo
  = info (helper <*> versioner <*> optionsParser)
  $ header "HTask.CLI"
  <> progDesc "track tasks in a local event log"

versioner :: Parser (a -> a)
versioner = infoOption (showVersion version)
  (  long "version"
  <> short 'v'
  <> help "Show version"
  )

getOptions :: IO Options
getOptions = do
  raw <- execParser optionsInfo
  file <- maybe ((</> ".tasks") <$> getHomeDirectory) pure (rawFile raw)
  pure $ Options (rawAction raw) file


-- | Dynamic Completer for Task IDs and UUIDs
taskCompleter :: Completer
taskCompleter = listIOCompleter $ do
  -- Try local .tasks first, then home
  localExists <- doesFileExist ".tasks"
  path <- if localExists
          then pure ".tasks"
          else (</> ".tasks") <$> getHomeDirectory

  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      evs <- V.runFileBackend path V.readEvents :: IO [V.Event H.TaskIntent]
      let (tasks, _) = H.foldEventLog evs
      let sorted = List.sortBy H.taskDisplayOrder (Map.elems tasks)
      let prefixes = H.disambiguatingPrefixes (Map.keys tasks)

      let sortedIndices = fmap (show . fst) (zip ([1..] :: [Int]) sorted)
      let uuids = fmap Text.unpack (Map.elems prefixes)

      pure (sortedIndices <> uuids)


-- | Command Parsers
addInfo :: ParserInfo Action
addInfo = info addParser (progDesc "Add a task description")
  where
    addParser = Add <$> argument str (metavar "DESCRIPTION")

completeInfo :: ParserInfo Action
completeInfo = info completeParser (progDesc "Mark a task as completed")
  where
    completeParser = Complete <$> argument str (metavar "TASKREF" <> completer taskCompleter)

doneInfo :: ParserInfo Action
doneInfo = info doneParser (progDesc "Marks the current task as completed")
  where
    doneParser = pure Done

dropInfo :: ParserInfo Action
dropInfo = info dropParser (progDesc "Drops the current tasks in progress")
  where
    dropParser = pure Drop

listInfo :: ParserInfo Action
listInfo = info listParser (progDesc "List tasks; optionally show removed tasks")
  where
    listParser
      = List . Tagged
      <$> switch
          (  long "show-uuid"
          <> short 'u'
          <> help "Show UUID for tasks"
          )
      <*> (Tagged <$> switch
          (  long "show-all"
          <> short 'a'
          <> help "Include completed and removed tasks"
          ) )

pickInfo :: ParserInfo Action
pickInfo = info pickParser (progDesc "Picks the next task by priority")
  where
    pickParser = pure Pick

removeInfo :: ParserInfo Action
removeInfo = info removeParser (progDesc "Removes a task from the list")
  where
    removeParser = Remove <$> argument str (metavar "TASKREF" <> completer taskCompleter)

startInfo :: ParserInfo Action
startInfo = info startParser (progDesc "Starts a given task")
  where
    startParser = Start <$> argument str (metavar "TASKREF" <> completer taskCompleter)

stopInfo :: ParserInfo Action
stopInfo = info stopParser (progDesc "Stops a given task")
  where
    stopParser = Stop <$> argument str (metavar "TASKREF" <> completer taskCompleter)

summaryInfo :: ParserInfo Action
summaryInfo = info summaryParser (progDesc "A short summary of current tasks")
  where
    summaryParser = pure Summary
