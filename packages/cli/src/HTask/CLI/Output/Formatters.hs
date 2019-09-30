{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Output.Formatters
  ( TermColor (..)
  , statusSymbol
  , withColor
  , withStatusColor
  , indent
  ) where

import qualified HTask.Core.Task as H

import           Data.Semigroup  (Semigroup, (<>))
import           Data.String     (IsString)


data TermColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Purple
  | Cyan
  | LightGrey
  | Default


resetColor :: (IsString s) => s
resetColor = "\x1b[0m"


termForeground :: (IsString s) => TermColor -> s
termForeground Black     = "\x1b[30m"
termForeground Red       = "\x1b[31m"
termForeground Green     = "\x1b[32m"
termForeground Yellow    = "\x1b[33m"
termForeground Blue      = "\x1b[34m"
termForeground Purple    = "\x1b[35m"
termForeground Cyan      = "\x1b[36m"
termForeground LightGrey = "\x1b[37m"
termForeground Default   = "\x1b[39m"


statusSymbol :: (IsString s) => H.TaskStatus -> s
statusSymbol H.Pending    = "•"
statusSymbol H.InProgress = "✭"
statusSymbol H.Complete   = "✓"
statusSymbol H.Abandoned  = "✕"


withColor :: (IsString s, Semigroup s) => TermColor -> s -> s
withColor c t = termForeground c <> t <> resetColor


withStatusColor :: (IsString s, Semigroup s) => H.TaskStatus -> s -> s
withStatusColor s = withColor (statusColor s)
  where
    statusColor H.Pending    = Blue
    statusColor H.InProgress = Yellow
    statusColor H.Complete   = Green
    statusColor H.Abandoned  = Red


indent :: (IsString s, Semigroup s) => s -> s
indent s = "  " <> s
