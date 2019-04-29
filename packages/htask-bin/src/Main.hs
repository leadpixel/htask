module Main
  ( main
  ) where

import qualified HTask.Runners as Runner
import qualified HTask.CLI as CLI
import HTask.Config
import HTask.Output.Renderers
import Event.Backend.File


main :: IO ()
main = do
  options <- CLI.getOptions
  let op = Runner.runAction (action options)
  k (taskfile options) (op) >>= renderResult


-- k :: FilePath -> FileBackend IO a -> IO a
k p op = runFileBackend op p
