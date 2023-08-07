module HTask.CLI.OptionsInfo
  ( addInfo
  , completeInfo
  , doneInfo
  , dropInfo
  , listInfo
  , pickInfo
  , removeInfo
  , startInfo
  , stopInfo
  , summaryInfo
  , summaryParser
  ) where

import           Data.Tagged         (Tagged (..))
import           HTask.CLI.Actions
import           Options.Applicative


addInfo :: ParserInfo Action
addInfo = info addParser (progDesc "Add a task description")
  where
    addParser :: Parser Action
    addParser = Add <$> argument str (metavar "DESCRIPTION")


completeInfo :: ParserInfo Action
completeInfo = info completeParser (progDesc "Mark a task as completed")
  where
    completeParser :: Parser Action
    completeParser = Complete <$> argument str (metavar "TASKREF")


doneInfo :: ParserInfo Action
doneInfo = info doneParser (progDesc "Marks the current task as completed")
  where
  doneParser :: Parser Action
  doneParser = pure Done


dropInfo :: ParserInfo Action
dropInfo = info dropParser (progDesc "Drops the current tasks in progress")
  where
    dropParser :: Parser Action
    dropParser = pure Drop


listInfo :: ParserInfo Action
listInfo = info listParser (progDesc "List tasks; optionally show removed tasks")
  where
    listParser :: Parser Action
    listParser
      = List
      <$> (Tagged <$> switch
          (  long "show-uuid"
          <> short 'u'
          <> help "Show UUID for tasks"
          ) )
      <*> (Tagged <$> switch
          (  long "show-all"
          <> short 'a'
          <> help "Include completed and removed tasks"
          ) )


pickInfo :: ParserInfo Action
pickInfo = info pickParser (progDesc "Picks the next task by priority")
  where
    pickParser :: Parser Action
    pickParser = pure Pick


removeInfo :: ParserInfo Action
removeInfo = info removeParser (progDesc "Removes a task from the list")
  where
    removeParser :: Parser Action
    removeParser = Remove <$> argument str (metavar "TASKREF")


startInfo :: ParserInfo Action
startInfo = info startParser (progDesc "Starts a given task")
  where
    startParser :: Parser Action
    startParser = Start <$> argument str (metavar "TASKREF")


stopInfo :: ParserInfo Action
stopInfo = info stopParser (progDesc "Stops a given task")
  where
    stopParser :: Parser Action
    stopParser = Stop <$> argument str (metavar "TASKREF")


summaryInfo :: ParserInfo Action
summaryInfo = info summaryParser (progDesc "A short summary of current tasks")


summaryParser :: Parser Action
summaryParser = pure Summary
