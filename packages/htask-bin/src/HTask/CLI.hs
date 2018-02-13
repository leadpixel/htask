module HTask.CLI
  ( getOptions
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

import HTask.Actions
import HTask.CLI.List
import HTask.CLI.Add
import HTask.CLI.Start
import HTask.CLI.Remove


defaultOption :: Parser Action
defaultOption = pure List


opts :: Parser Action
opts = hsubparser
  (  command "list" listInfo
  <> command "add" addInfo
  <> command "start" startInfo
  <> command "remove" removeInfo
  )


getOptions :: IO Action
getOptions = execParser
  ( info (opts <**> helper <|> defaultOption)
  $ progDesc "f"
  )
