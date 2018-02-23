{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Done
  ( runDone
  ) where

import HTask.TaskApplication
import HTask.Output
import qualified HTask as H
import Control.Monad
import Data.Semigroup ((<>))


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runDone :: TaskConfig Output
runDone
  = runTask H.listTasks
  >>= fmap join . mapM completeTask . filter (hasStatus H.InProgress)

  where
    completeTask t = do
      _ <- runTask $ H.completeTask $ H.taskRef t
      pure ([ line $ "completing task: " <> H.description t ])
