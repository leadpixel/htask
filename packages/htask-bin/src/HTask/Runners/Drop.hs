{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Drop
  ( runDrop
  ) where

import Control.Monad.IO.Class
import Event
import HTask.TaskApplication
import HTask.Output
import qualified HTask as H
import Control.Monad
import Data.Semigroup ((<>))


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runDrop :: (H.HasTasks (TaskApplication m), H.CanCreateTask m, MonadIO m) => EventBackend m Document
runDrop
  = Document <$>
    ( runTask H.listTasks
    >>= fmap join . mapM stopTask . filter (hasStatus H.InProgress)
    )


stopTask t = do
  _ <- runTask $ H.stopTask $ H.taskRef t
  pure [ line $ "stopping task: " <> H.description t ]
