{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Drop
  ( runDrop
  ) where

import qualified HTask as H

import Control.Monad
import HTask.Output
import HTask.TaskApplication

import Data.Semigroup ((<>))


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runDrop :: (HasEventBackend m, H.CanCreateTask m) => m Document
runDrop
  = Document <$>
    ( runTask H.listTasks
    >>= fmap join . mapM execStopTask . filter (hasStatus H.InProgress)
    )

  where

    execStopTask t = do
      _ <- runTask $ H.stopTask $ H.taskRef t
      pure [ line $ "stopping task: " <> H.description t ]
