module Main (main) where

import qualified HTask.CLI.Options as Opt
import qualified HTask.CLI.Output  as Render
import qualified HTask.CLI.Runners as Runner

import           HTask.CLI.App


main :: IO ()
main = do
  options <- Opt.getOptions
  let file = Opt.taskfile options

  runApp file (Runner.runAction options) >>= Render.renderResult
