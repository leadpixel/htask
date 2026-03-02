{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Output
  ( RunResult
  , indent
  , renderResult
  , resultError
  , resultSuccess
  , statusSymbol
  , text
  , withStatusColor
  ) where

import qualified Data.Text  as Text
import qualified HTask.Core as H

import           Data.Text  (Text)


-- | Rendering Pipeline Types
data RunResult
  = Success [Text]
  | Error [Text]
  deriving (Eq, Show)

resultSuccess :: [Text] -> RunResult
resultSuccess = Success

resultError :: Text -> RunResult
resultError t = Error [t]

text :: RunResult -> [Text]
text (Success ts) = formatSuccess ts
text (Error ts)   = formatError ts


-- | IO Rendering
renderResult :: RunResult -> IO ()
renderResult = mapM_ (putStrLn . Text.unpack) . text


-- | Formatting Logic
data Color = Red | Green | Yellow | Blue | Normal

withColor :: Color -> String -> String
withColor Red    s = "\ESC[31m" <> s <> "\ESC[0m"
withColor Green  s = "\ESC[32m" <> s <> "\ESC[0m"
withColor Yellow s = "\ESC[33m" <> s <> "\ESC[0m"
withColor Blue   s = "\ESC[34m" <> s <> "\ESC[0m"
withColor Normal s = s

withStatusColor :: H.TaskStatus -> Text -> Text
withStatusColor H.InProgress = Text.pack . withColor Blue . Text.unpack
withStatusColor H.Pending    = Text.pack . withColor Yellow . Text.unpack
withStatusColor H.Complete   = Text.pack . withColor Green . Text.unpack
withStatusColor H.Abandoned  = Text.pack . withColor Red . Text.unpack

statusSymbol :: H.TaskStatus -> Text
statusSymbol H.InProgress = "•"
statusSymbol H.Pending    = "•"
statusSymbol H.Complete   = "✔"
statusSymbol H.Abandoned  = "✘"

indent :: Text -> Text
indent t = "  " <> t

formatError :: [Text] -> [Text]
formatError t = [ Text.pack $ withColor Red "Error" <> ":" ] <> t

formatSuccess :: [Text] -> [Text]
formatSuccess t = [ Text.pack $ withColor Green "Success!" ] <> t
