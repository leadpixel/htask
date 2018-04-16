{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.Runners.Pick
  ( runPick
  ) where

import Control.Monad.IO.Class
import Event
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


runPick :: (H.CanCreateTask m, MonadIO m, H.HasTasks (TaskApplication m)) => EventBackend m Document
runPick = do
  ts <- runTask H.listTasks
  let ps = filter (hasStatus H.Pending) ts
  k <- lift $ liftIO $ randomSelectOne ps
  Document <$> maybe
    emptyMessage
    startTask
    k

  where
    startTask t = do
      _ <- runTask $ H.startTask $ H.taskRef t
      pure [ line ("picking task: " <> H.description t)]


    emptyMessage :: (Applicative m) => m [Block]
    emptyMessage
      = pure [ line "no task to pick" ]


randomSelectOne :: (CanRandom m) => [a] -> m (Maybe a)
randomSelectOne [] = pure Nothing
randomSelectOne xs = do
  n <- getRandomR (0, length xs)
  pure $ Just $ xs !! n
