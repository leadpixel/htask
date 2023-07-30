{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Pick
  ( runPick
  ) where


import qualified Data.Sequence              as Seq
import qualified HTask.Core                 as H

import           Control.Monad.Random.Class (MonadRandom (getRandomR))
import           Data.Sequence              (Seq, (!?))
import           Data.Text                  (Text)
import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


taskToText :: H.Task -> Text
taskToText = H.taskUuidToText . H.taskUuid


runPick :: (MonadRandom m, HasEventBackend m, H.CanCreateTask m) => m RunResult
runPick = do
  ts <- runTask H.listTasks
  let ps = Seq.filter (hasStatus H.Pending) ts
  k <- randomSelectOne ps
  maybe
    (pure $ resultError "no task to pick")
    (fmap resultSuccess . startTask)
    k

  where
    startTask t = do
      _ <- runTask (H.startTask $ taskToText t)
      pure ["picking task: " <> H.description t]


randomSelectOne :: (Monad m, MonadRandom m) => Seq a -> m (Maybe a)
randomSelectOne Seq.Empty = pure Nothing
randomSelectOne xs =
  (xs !?) <$> getRandomR (0, Seq.length xs)
