module HTask.Output.Renderers
  ( renderResult
  ) where

import qualified Data.Text               as Text

import           HTask.Output.Document
import           HTask.Renderers.Console


renderResult :: RunResult -> IO ()
renderResult = mapM_ (putStrLn . Text.unpack) . renderToConsole
