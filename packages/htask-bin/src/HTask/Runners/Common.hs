{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Common
  ( withMatch
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))


headSafe :: [a] -> Maybe a
headSafe [x] = Just x
headSafe _ = Nothing


findMatch :: (HasEventBackend m) => Text.Text -> m (Maybe H.Task)
findMatch ref
  =   headSafe . filterMatchesUUID ref
  <$> runTask H.listTasks

 where
   filterMatchesUUID :: Text.Text -> [H.Task] -> [H.Task]
   filterMatchesUUID t
     = filter (Text.isPrefixOf t . H.taskRefText . H.taskRef)


withMatch :: (HasEventBackend m) => (H.Task -> m Document) -> Text.Text -> m Document
withMatch op t
  = findMatch t
  >>= maybe (pure $ formatErrorMatch t) op

  where
    formatErrorMatch t'
      = formatError ("did not find unique match for " <> t')
