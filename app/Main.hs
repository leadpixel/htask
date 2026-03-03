module Main (main) where

import qualified HTask.CLI.Options as Options
import qualified HTask.CLI.Output  as Output
import qualified HTask.CLI.Runners as Runners

import           HTask.CLI.App

main :: IO ()
main = do
  options <- Options.getOptions
  let file = Options.taskfile options

  runApp file (Runners.runAction options) >>= Output.renderResult
