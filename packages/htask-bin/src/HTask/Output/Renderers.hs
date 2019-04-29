module HTask.Output.Renderers
  ( renderResult
  ) where

import HTask.Output.Document
import HTask.Renderers.Console


renderResult :: RunResult -> IO ()
renderResult = print . renderToConsole
