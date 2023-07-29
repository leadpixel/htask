module HTask.CLI.Render
  ( renderResult
  ) where

import qualified Data.Text                   as Text
import           HTask.CLI.Output.Document   (RunResult (..))
import           HTask.CLI.Output.Formatters (TermColor (..), withColor)

import           Data.Text                   (Text)


renderResult :: RunResult -> IO ()
renderResult = mapM_ (putStrLn . Text.unpack) . renderToConsole


renderToConsole :: RunResult -> [Text]
renderToConsole r
  = ( if success r
       then formatSuccess
       else formatError ) (text r)


formatError :: [Text] -> [Text]
formatError t =
  [ Text.pack $ withColor Red "Error" <> ":" ] <> t


formatSuccess :: [Text] -> [Text]
formatSuccess t =
  [ Text.pack $ withColor Green "Success!" ] <> t
