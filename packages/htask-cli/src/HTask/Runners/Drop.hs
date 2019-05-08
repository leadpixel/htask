{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Drop
  ( runDrop
  ) where


import qualified HTask.API             as API
import qualified HTask.Task            as H

import           HTask.Output.Document
import           HTask.TaskApplication

import           Control.Monad         (join)
import           Data.Semigroup        ((<>))


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runDrop :: (HasEventBackend m, H.CanCreateTask m) => m RunResult
runDrop
  = resultSuccess <$>
    ( runTask API.listTasks
    >>= fmap join . mapM execStopTask . filter (hasStatus H.InProgress)
    )

  where

    execStopTask t = do
      _ <- runTask $ API.stopTask $ H.taskRef t
      pure [ "stopping task: " <> H.description t ]
