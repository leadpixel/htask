module Main
  ( main
  ) where

import HTask.Runners
import HTask.CLI
import HTask.TaskApplication


main :: IO ()
main = do
  options <- getOptions

  runCommand options "tasks.txt"
