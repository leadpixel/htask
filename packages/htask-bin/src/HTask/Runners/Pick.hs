{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Pick
  ( runPick
  ) where

import System.Random
import Control.Monad.Reader
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))
import qualified HTask as H


class (Monad m) => CanRandom m where
  getRandomR :: (Random a) => (a, a) -> m a


instance CanRandom IO where
  getRandomR = randomRIO


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runPick :: TaskConfig Output
runPick = do
  ts <- runTask H.listTasks
  let ps = filter (hasStatus H.Pending) ts
  k <- lift $ randomSelectOne ps
  pure <$> maybe
    (pure $ line "no task to pick")
    startTask
    k

  where
    startTask t = do
      _ <- runTask $ H.startTask $ H.taskRef t
      pure $ line ("picking task: " <> H.description t)


randomSelectOne :: (CanRandom m) => [a] -> m (Maybe a)
randomSelectOne [] = pure Nothing
randomSelectOne xs = do
  n <- getRandomR (0, length xs)
  pure $ Just $ xs !! n
