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


runDone :: TaskConfig Document
runDone
  = Document
  <$> ( runTask H.listTasks
      >>= fmap join . mapM completeTask . filter (hasStatus H.InProgress)
      )

  where
    completeTask :: H.Task -> TaskConfig [Block]
    completeTask t = do
      _ <- execDone t
      pure $ formatDone t


execDone :: H.Task -> TaskConfig (Either String H.TaskRef)
execDone
  = runTask . H.completeTask . H.taskRef


formatDone :: H.Task -> [Block]
formatDone t
  = [ line $ "completing task: " <> H.description t ]
