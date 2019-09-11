module HTask.CLI.Actions.List
  where

import           Data.Semigroup      ((<>))
import           HTask.CLI.Actions
import           Options.Applicative


listInfo :: ParserInfo Action
listInfo = info listParser (progDesc "List tasks; optionally show removed tasks")


listParser :: Parser Action
listParser
  = List
  <$> switch
      (  long "show-uuid"
      <> short 'u'
      <> help "Show UUID for tasks"
      )
  <*> switch
      (  long "show-all"
      <> short 'a'
      <> help "Include completed and removed tasks"
      )
