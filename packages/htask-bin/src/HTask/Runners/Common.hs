{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Common
  ( withMatch
  ) where

import Control.Monad.IO.Class
import Event
import qualified Data.Text as Text
import qualified HTask as H
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))


headSafe :: [a] -> Maybe a
headSafe [x] = Just x
headSafe _ = Nothing


findMatch ref
  =   headSafe . filterMatchesUUID ref
  <$> runTask H.listTasks

 where
   filterMatchesUUID :: Text.Text -> [H.Task] -> [H.Task]
   filterMatchesUUID t
     = filter (Text.isPrefixOf t . H.taskRefText . H.taskRef)


withMatch op t
  = findMatch t
  >>= maybe (pure $ formatErrorMatch t) op

  where
    formatErrorMatch t'
      = formatError ("did not find unique match for " <> t')
