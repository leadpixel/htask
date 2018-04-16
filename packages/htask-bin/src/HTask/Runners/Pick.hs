{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.Runners.Pick
  ( runPick
  ) where

import Control.Monad.IO.Class
import Event
import Control.Monad.Reader
import HTask.TaskApplication
import HTask.Output
import Data.Semigroup ((<>))
import qualified HTask as H


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
  n <- getRandomRange (0, length xs)
  pure $ Just $ xs !! n
