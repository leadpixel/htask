module HTask.Renderers.Console
  ( renderToConsole
  ) where


import Data.Text (Text, pack)
import HTask.Output.Document
import HTask.Output.Formatters


renderToConsole :: RunResult -> [Text]
renderToConsole r
  = case (success r) of
      True -> formatSuccess (text r)
      False -> formatError (text r)


formatError :: [Text] -> [Text]
formatError t =
  [ pack $ withColor Red "Error" <> ":" ] <> t


formatSuccess :: [Text] -> [Text]
formatSuccess t =
  [ pack $ withColor Green "Success!" ] <> t

