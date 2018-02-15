{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Pick
  ( runPick
  ) where

import Control.Monad.Reader
import HTask.TaskApplication
import Data.Semigroup ((<>))
import qualified HTask as H
import qualified Data.Text as Text


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runPick :: TaskConfig ()
runPick = do
  ts <- runTask H.listTasks
  let ps = filter (hasStatus H.Pending) ts
  maybe
    (lift (putStrLn "no task to pick"))
    (startTask)
    (randomSelectOne ps)
  pure ()

  where
    startTask t = do
      _ <- runTask $ H.startTask $ H.taskRef t
      lift $ putStrLn (Text.unpack $ "picking task: " <> H.description t)


randomSelectOne :: [a] -> Maybe a
randomSelectOne [] = Nothing
randomSelectOne (x:_) = Just x
