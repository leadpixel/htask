{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Summary
  ( runSummary
  ) where

import Data.List
import Data.Function
import Control.Monad.Reader
import Data.Semigroup ((<>))
import Data.Tagged
import qualified Data.UUID as UUID
import HTask.TaskApplication
import HTask.Formatters
import qualified Data.Text              as Text
import qualified HTask as H


taskPriority :: H.Task -> H.Task -> Ordering
taskPriority = compare `on` H.createdAt


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runSummary :: TaskConfig ()
runSummary
  = runTask H.listTasks
  >>= \ts -> lift $ do
      displayCurrent ts
      putStrLn ""
      displayTopPending ts


displayCurrent :: [H.Task] -> IO ()
displayCurrent ts = do
  let ps = filter (hasStatus H.InProgress) ts
  if Data.List.null ps
     then putStrLn "No current task"
     else do
       putStrLn "Current task:"
       mapM_ printTaskForSummary ps


displayTopPending :: [H.Task] -> IO ()
displayTopPending ts = do
  let ps = sortBy taskPriority (filter (hasStatus H.Pending) ts)
  let xs = take 5 ps
  putStrLn (pendingMessage (length xs) (length ps))
  mapM_ printTaskForSummary xs

  where
    pendingMessage x p
      = mconcat
        [ "Top " , show x , " pending"
        , " (" , show (p - x) , " hidden):"
        ]



printTaskForSummary :: H.Task -> IO ()
printTaskForSummary t = do
  putStrLn (Text.unpack $ indent $ printDescription t)
  putStrLn (Text.unpack $ indent $ indent $ printRef t)

  where
    printDescription t
      =  statusSymbol (H.status t)
      <> " "
      <> withStatusColor (H.status t) (H.description t)

    printRef = UUID.toText . untag . H.taskRef
