{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Pick
  ( runPick
  ) where

import System.Random
import Control.Monad.Reader
import HTask.TaskApplication
import Data.Semigroup ((<>))
import qualified HTask as H
import qualified Data.Text as Text


class (Monad m) => CanRandom m where
  getRandomR :: (Random a) => (a, a) -> m a


instance CanRandom IO where
  getRandomR = randomRIO


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runPick :: TaskConfig ()
runPick = do
  ts <- runTask H.listTasks
  let ps = filter (hasStatus H.Pending) ts
  k <- lift $ randomSelectOne ps
  maybe
    (lift $ putStrLn "no task to pick")
    startTask
    k
  pure ()

  where
    startTask t = do
      _ <- runTask $ H.startTask $ H.taskRef t
      lift $ putStrLn (Text.unpack $ "picking task: " <> H.description t)


randomSelectOne :: (CanRandom m) => [a] -> m (Maybe a)
randomSelectOne [] = pure Nothing
randomSelectOne xs = do
  n <- getRandomR (0, length xs)
  pure $ Just $ xs !! n
