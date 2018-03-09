module HTask.CLI.List
  where

import HTask.Actions
import Options.Applicative
import Data.Semigroup ((<>))


listInfo :: ParserInfo Action
listInfo = info listParser (progDesc "list tasks")


listParser :: Parser Action
listParser
  = List
  <$> switch
      (  long "show-uuid"
      <> short 'u'
      <> help "Show UUID for tasks"
      )
  <*> switch
      (  long "all"
      <> short 'a'
      <> help "Show all tasks"
      )
