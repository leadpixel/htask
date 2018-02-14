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
  <$> flag HideDetail ShowDetail
      (  long "detail"
      <> short 'd'
      <> help "show detail"
      )
