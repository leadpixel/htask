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
  <$> flag False True
      (  long "show-uuid"
      <> short 'u'
      <> help "Show UUID for tasks"
      )
  <*> flag False True
      (  long "include-deleted"
      <> short 'a'
      <> help "Include deleted tasks"
      )
