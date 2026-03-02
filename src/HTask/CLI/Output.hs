{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Output
  ( RunResult
  , indent
  , padLeft
  , renderResult
  , resultError
  , resultSuccess
  , statusSymbol
  , text
  , withBold
  , withDim
  , withStatusColor
  ) where

import qualified Data.Text        as Text
import qualified HTask.Core       as H

import           Data.IORef
import           Data.Text        (Text)
import           System.IO        (hIsTerminalDevice, stdout)
import           System.IO.Unsafe (unsafePerformIO)


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
data Color = Red | Green | Yellow | Blue | Cyan | White

withColor :: Color -> String -> String
withColor c s
  | shouldUseColor =
      case c of
        Red    -> "\ESC[31m" <> s <> "\ESC[0m"
        Green  -> "\ESC[32m" <> s <> "\ESC[0m"
        Yellow -> "\ESC[33m" <> s <> "\ESC[0m"
        Blue   -> "\ESC[34m" <> s <> "\ESC[0m"
        Cyan   -> "\ESC[36m" <> s <> "\ESC[0m"
        White  -> "\ESC[37m" <> s <> "\ESC[0m"
  | otherwise = s

withBold :: Text -> Text
withBold t
  | shouldUseColor = Text.pack $ "\ESC[1m" <> Text.unpack t <> "\ESC[0m"
  | otherwise = t

withDim :: Text -> Text
withDim t
  | shouldUseColor = Text.pack $ "\ESC[2m" <> Text.unpack t <> "\ESC[0m"
  | otherwise = t

withStatusColor :: H.TaskStatus -> Text -> Text
withStatusColor H.InProgress = Text.pack . withColor Cyan . Text.unpack
withStatusColor H.Pending    = Text.pack . withColor Yellow . Text.unpack
withStatusColor H.Complete   = Text.pack . withColor Green . Text.unpack
withStatusColor H.Abandoned  = Text.pack . withColor Red . Text.unpack

statusSymbol :: H.TaskStatus -> Text
statusSymbol H.InProgress = "▶"
statusSymbol H.Pending    = "○"
statusSymbol H.Complete   = "✔"
statusSymbol H.Abandoned  = "✘"

indent :: Text -> Text
indent t = "  " <> t

padLeft :: Int -> Text -> Text
padLeft n t = Text.replicate (n - Text.length t) " " <> t

formatError :: [Text] -> [Text]
formatError t = [ Text.pack (withColor Red "Error" <> ":") ] <> t

formatSuccess :: [Text] -> [Text]
formatSuccess t = [ Text.pack (withColor Green "Success!") ] <> t
