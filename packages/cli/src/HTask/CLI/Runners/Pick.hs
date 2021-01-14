{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Pick
  ( runPick
  ) where


import           Control.Monad.Random.Class
import qualified Data.UUID                  as UUID
import qualified HTask.Core.API             as API
import qualified HTask.Core.Task            as H

import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication

import           Data.Tagged                (untag)
import           Data.Text                  (Text)


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


taskRefText :: H.Task -> Text
taskRefText = UUID.toText . untag . H.taskRef


runPick :: (MonadRandom m, HasEventBackend m, H.CanCreateTask m) => m RunResult
runPick = do
  ts <- runTask API.listTasks
  let ps = filter (hasStatus H.Pending) ts
  k <- randomSelectOne ps
  maybe
    (pure $ resultError "no task to pick")
    (fmap resultSuccess . startTask)
    k

  where
    startTask t = do
      _ <- runTask (API.startTask $ taskRefText t)
      pure ["picking task: " <> H.description t]


randomSelectOne :: (Monad m, MonadRandom m) => [a] -> m (Maybe a)
randomSelectOne [] = pure Nothing
randomSelectOne xs =
  (\n -> Just $ xs !! n) <$> getRandomR (0, length xs)
