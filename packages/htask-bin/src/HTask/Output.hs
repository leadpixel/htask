module HTask.Output
  ( Output
  , line
  , renderOutput
  ) where

import qualified Data.Text as Text
import HTask.Config
import qualified Data.Aeson as Aeson


type Output = [Block]


data Block
  = Line Text.Text


line :: Text.Text -> Block
line = Line


unLine :: Block -> String
unLine (Line t) = Text.unpack t


renderOutput :: Formatter -> Output -> IO ()
renderOutput Default = renderToConsole
renderOutput JSON = renderToJSON
renderOutput Porcelain = renderToPorcelain


renderToConsole :: Output -> IO ()
renderToConsole = mapM_ (putStrLn . unLine)


renderToJSON :: Output -> IO ()
renderToJSON = print . Aeson.encode . fmap unLine


renderToPorcelain :: Output -> IO ()
renderToPorcelain = renderToConsole
