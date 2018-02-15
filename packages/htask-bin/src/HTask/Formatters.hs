{-# LANGUAGE OverloadedStrings #-}

module HTask.Formatters
  where

import qualified HTask as H
import Data.Semigroup (Semigroup, (<>))
import Data.String (IsString)


statusSymbol :: (IsString s) => H.TaskStatus -> s
statusSymbol H.Pending    = "•"
statusSymbol H.InProgress = "✭"
statusSymbol H.Complete   = "✓"
statusSymbol H.Abandoned  = "✕"


withStatusColor :: (IsString s, Semigroup s) => H.TaskStatus -> s -> s
withStatusColor s t = statusColor s <> t <> "\x1b[0m"  -- Clear
  where
    statusColor H.Pending    = "\x1b[34m" -- Blue
    statusColor H.InProgress = "\x1b[33m" -- Yellow
    statusColor H.Complete   = "\x1b[32m" -- Green
    statusColor H.Abandoned  = "\x1b[31m" -- Red


indent :: (IsString s, Semigroup s) => s -> s
indent s = "  " <> s
