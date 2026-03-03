{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Output
  ( RunResult
  , divider
  , indent
  , padLeft
  , padZero
  , renderResult
  , resultError
  , resultJson
  , resultSuccess
  , statusSymbol
  , text
  , treeLink
  , withBold
  , withDim
  , withStatusColor
  ) where

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as AesonPretty
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as Text
import qualified HTask.Core                 as Core

import           Data.IORef
import           Data.Text                  (Text)
import           System.IO                  (hIsTerminalDevice, stdout)
import           System.IO.Unsafe           (unsafePerformIO)


-- | Rendering Pipeline Types
data RunResult
  = Success [Text]
  | Json Aeson.Value
  | Error [Text]
  deriving (Eq, Show)

resultSuccess :: [Text] -> RunResult
resultSuccess = Success

resultJson :: (Aeson.ToJSON a) => a -> RunResult
resultJson = Json . Aeson.toJSON

resultError :: Text -> RunResult
resultError t = Error [t]

text :: RunResult -> [Text]
text (Success ts) = formatSuccess ts
text (Error ts)   = formatError ts
text (Json v)     = [Text.pack $ LBS.unpack $ AesonPretty.encodePretty v]


-- | Global coloring state
{-# NOINLINE useColorRef #-}
useColorRef :: IORef Bool
useColorRef = unsafePerformIO $ newIORef True

setUseColor :: Bool -> IO ()
setUseColor = writeIORef useColorRef

{-# NOINLINE shouldUseColor #-}
shouldUseColor :: Bool
shouldUseColor = unsafePerformIO $ readIORef useColorRef


-- | IO Rendering
renderResult :: RunResult -> IO ()
renderResult res = do
  useColor <- hIsTerminalDevice stdout
  setUseColor useColor
  mapM_ (putStrLn . Text.unpack) (text res)


-- | Formatting Logic
data Color = Red | Green | Yellow | Blue | Cyan | Gray

withColor :: Color -> String -> String
withColor c s
  | shouldUseColor =
      case c of
        Red    -> "\ESC[31m" <> s <> "\ESC[0m"
        Green  -> "\ESC[32m" <> s <> "\ESC[0m"
        Yellow -> "\ESC[33m" <> s <> "\ESC[0m"
        Blue   -> "\ESC[34m" <> s <> "\ESC[0m"
        Cyan   -> "\ESC[36m" <> s <> "\ESC[0m"
        Gray   -> "\ESC[90m" <> s <> "\ESC[0m"
  | otherwise = s

withBold :: Text -> Text
withBold t
  | shouldUseColor = Text.pack $ "\ESC[1m" <> Text.unpack t <> "\ESC[0m"
  | otherwise = t

withDim :: Text -> Text
withDim = Text.pack . withColor Gray . Text.unpack

withStatusColor :: Core.TaskStatus -> Text -> Text
withStatusColor Core.InProgress = Text.pack . withColor Cyan . Text.unpack
withStatusColor Core.Pending    = Text.pack . withColor Yellow . Text.unpack
withStatusColor Core.Complete   = Text.pack . withColor Green . Text.unpack
withStatusColor Core.Abandoned  = Text.pack . withColor Red . Text.unpack

statusSymbol :: Core.TaskStatus -> Text
statusSymbol Core.InProgress = "▶"
statusSymbol Core.Pending    = "○"
statusSymbol Core.Complete   = "✔"
statusSymbol Core.Abandoned  = "✘"

indent :: Text -> Text
indent t = "  " <> t

padLeft :: Int -> Text -> Text
padLeft n t = Text.replicate (n - Text.length t) " " <> t

padZero :: Int -> Text -> Text
padZero n t = Text.replicate (n - Text.length t) "0" <> t

divider :: Text -> Text
divider label = withBold label <> " " <> withDim (Text.replicate (60 - Text.length label) "─")

treeLink :: Text
treeLink = "╰─ "

formatError :: [Text] -> [Text]
formatError t = [ Text.pack (withColor Red "Error" <> ":") ] <> t

formatSuccess :: [Text] -> [Text]
formatSuccess t = [ Text.pack (withColor Green "Success!") ] <> t
