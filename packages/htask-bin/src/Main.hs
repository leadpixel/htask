module Main
  ( main
  ) where

import qualified HTask.Runners as Runner
import qualified HTask.CLI as CLI


main :: IO ()
main = do
  options <- CLI.getOptions
  Runner.runCommand options
