{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Done
  ( runDone
  ) where

import HTask.TaskApplication
import qualified HTask as H
import qualified Data.Text as Text
import Control.Monad.Trans
import Data.Semigroup ((<>))


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runDone :: TaskConfig ()
runDone
  = runTask H.listTasks
  >>= mapM_ completeTask . filter (hasStatus H.InProgress)

  where
    completeTask t = do
      _ <- runTask $ H.completeTask $ H.taskRef t
      lift $ putStrLn (Text.unpack $ "completing task: " <> H.description t)
