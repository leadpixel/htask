{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Pick
  ( runPick
  ) where

import qualified HTask                 as H

import           Event
import           HTask.Output
import           HTask.TaskApplication

import           Data.Semigroup        ((<>))


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runPick :: (CanRandom m, HasEventBackend m, H.CanCreateTask m) => m RunResult
runPick = do
  ts <- runTask H.listTasks
  let ps = filter (hasStatus H.Pending) ts
  k <- randomSelectOne ps
  maybe
    (pure $ resultError "no task to pick")
    (\x -> resultSuccess <$> startTask x)
    k

  where
    startTask t = do
      _ <- runTask $ H.startTask $ H.taskRef t
      pure [("picking task: " <> H.description t)]


randomSelectOne :: (Monad m, CanRandom m) => [a] -> m (Maybe a)
randomSelectOne [] = pure Nothing
randomSelectOne xs =
  (\n -> Just $ xs !! n) <$> getRandomRange (0, length xs)
