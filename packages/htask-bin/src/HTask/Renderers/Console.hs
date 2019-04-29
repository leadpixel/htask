module HTask.Renderers.Console
  ( renderToConsole
  ) where


import           Data.Text               (Text, pack)
import           HTask.Output.Document
import           HTask.Output.Formatters


renderToConsole :: RunResult -> [Text]
renderToConsole r
  = if success r then formatSuccess (text r) else formatError (text r)


formatError :: [Text] -> [Text]
formatError t =
  [ pack $ withColor Red "Error" <> ":" ] <> t


formatSuccess :: [Text] -> [Text]
formatSuccess t =
  [ pack $ withColor Green "Success!" ] <> t
