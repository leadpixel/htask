module HTask.Output.Renderers
  ( renderDocument
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import HTask.Config
import HTask.Output.Document


unLine :: Block -> String
unLine (Line t)
  = Text.unpack t


renderDocument :: Formatter -> Document -> IO ()
renderDocument Terminal  = renderToConsole
renderDocument JSON      = renderToJSON
renderDocument Porcelain = renderToPorcelain


renderToConsole :: Document -> IO ()
renderToConsole
  = mapM_ (putStrLn . unLine) . undoc


renderToJSON :: Document -> IO ()
renderToJSON
  = print . Aeson.encode . fmap unLine . undoc


renderToPorcelain :: Document -> IO ()
renderToPorcelain
  = renderToConsole
