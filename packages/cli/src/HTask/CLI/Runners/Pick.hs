{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners.Pick
  ( runPick
  ) where


import qualified Data.UUID                 as UUID
import qualified Effects                   as F
import qualified HTask.Core.API            as API
import qualified HTask.Core.Task           as H

import           HTask.CLI.Output.Document
import           HTask.CLI.TaskApplication

import           Data.Semigroup            ((<>))
import           Data.Tagged               (untag)
import           Data.Text                 (Text)


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


taskRefText :: H.Task -> Text
taskRefText = UUID.toText . untag . H.taskRef


runPick :: (F.CanRandom m, HasEventBackend m, H.CanCreateTask m) => m RunResult
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


randomSelectOne :: (Monad m, F.CanRandom m) => [a] -> m (Maybe a)
randomSelectOne [] = pure Nothing
randomSelectOne xs =
  (\n -> Just $ xs !! n) <$> F.getRandomRange (0, length xs)
