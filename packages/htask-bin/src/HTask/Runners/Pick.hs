{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Pick
  ( runPick
  ) where


import qualified Effects               as F
import qualified HTask.API             as API
import qualified HTask.Task            as H

import           HTask.Output.Document
import           HTask.TaskApplication

import           Data.Semigroup        ((<>))


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


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
      _ <- runTask $ API.startTask $ H.taskRef t
      pure ["picking task: " <> H.description t]


randomSelectOne :: (Monad m, F.CanRandom m) => [a] -> m (Maybe a)
randomSelectOne [] = pure Nothing
randomSelectOne xs =
  (\n -> Just $ xs !! n) <$> F.getRandomRange (0, length xs)
