{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Drop
  ( runDrop
  ) where

import HTask.TaskApplication
import qualified HTask as H
import qualified Data.Text as Text
import Control.Monad.Trans
import Data.Semigroup ((<>))


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runDrop :: TaskConfig ()
runDrop
  = runTask H.listTasks
  >>= mapM_ stopTask . filter (hasStatus H.InProgress)

  where
    stopTask t = do
      _ <- runTask $ H.stopTask $ H.taskRef t
      lift $ putStrLn (Text.unpack $ "stopping task: " <> H.description t)
