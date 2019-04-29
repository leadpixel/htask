{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Common
  ( withMatch
  ) where

import           Data.Semigroup        ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified HTask                 as H
import           HTask.Output
import           HTask.TaskApplication


headSafe :: [a] -> Maybe a
headSafe [x] = Just x
headSafe _   = Nothing


findMatch :: (HasEventBackend m) => Text -> m (Maybe H.Task)
findMatch ref
  =   headSafe . filterMatchesUUID ref
  <$> runTask H.listTasks

 where
   filterMatchesUUID :: Text -> [H.Task] -> [H.Task]
   filterMatchesUUID t
     = filter (Text.isPrefixOf t . H.taskRefText . H.taskRef)


withMatch :: (HasEventBackend m) => (H.Task -> m RunResult) -> Text -> m RunResult
withMatch op t
  = findMatch t
  >>= maybe (pure $ formatErrorMatch t) op

  where
    formatErrorMatch t'
      = resultError $ "did not find unique match for " <> t'
