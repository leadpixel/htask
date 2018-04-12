{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Drop
  ( runDrop
  ) where

import HTask.TaskApplication
import HTask.Output
import qualified HTask as H
import Control.Monad
import Data.Semigroup ((<>))


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runDrop :: TaskConfig IO Document
runDrop
  = Document <$>
    ( runTask H.listTasks
    >>= fmap join . mapM stopTask . filter (hasStatus H.InProgress)
    )


stopTask :: H.Task -> TaskConfig IO [Block]
stopTask t = do
  _ <- runTask $ H.stopTask $ H.taskRef t
  pure [ line $ "stopping task: " <> H.description t ]
