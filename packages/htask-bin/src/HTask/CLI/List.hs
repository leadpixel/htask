module HTask.CLI.List
  where

import HTask.Actions
import Options.Applicative
import Data.Semigroup ((<>))


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
      (  long "include-removed"
      <> short 'a'
      <> help "Include removed tasks"
      )
