module HTask.Runners.Summary
  ( runSummary
  ) where

import Data.List
import Data.Function
import Control.Monad.Reader
import Data.Semigroup ((<>))
import Data.Tagged
import Data.UUID
import HTask.TaskApplication
import qualified Data.Text              as Text
import qualified HTask as H


taskPriority :: H.Task -> H.Task -> Ordering
taskPriority = compare `on` H.createdAt


runSummary :: TaskConfig ()
runSummary
  = runTask H.listTasks
  >>= \ts -> lift $ do
      displayCurrent ts
      putStrLn ""
      displayTopPending ts


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


displayCurrent :: [H.Task] -> IO ()
displayCurrent ts = do
  let ps = filter (hasStatus H.InProgress) ts
  if Data.List.null ps
     then putStrLn "No current task"
     else do
       putStrLn "Current task:"
       mapM_ showCurrentTask ps

  where
    showCurrentTask t = do
      putStrLn ("  > " <> showDescription t)
      putStrLn ("    " <> showRef t)


showDescription :: H.Task -> String
showDescription t
  =  statusColor (Just $ H.status t)
  <> Text.unpack (H.description t)
  <> statusColor Nothing


showRef :: H.Task -> String
showRef = toString . untag . H.taskRef


displayTopPending :: [H.Task] -> IO ()
displayTopPending ts = do
  let ps = sortBy taskPriority (filter (hasStatus H.Pending) ts)
  let xs = take 5 ps
  putStrLn (pendingMessage (length xs) (length ps))
  mapM_ showPendingTask xs

  where
    pendingMessage x p
      = mconcat
        [ "Top " , show x , " pending"
        , " (" , show (p - x) , " hidden):"
        ]

    showPendingTask t = do
      putStrLn ("  . " <> showDescription t)
      putStrLn ("    " <> showRef t)


statusColor :: Maybe H.TaskStatus -> String
statusColor Nothing             = "\x1b[0m"  -- Clear
statusColor (Just H.Pending)    = "\x1b[34m" -- Blue
statusColor (Just H.InProgress) = "\x1b[33m" -- Yellow
statusColor (Just H.Complete)   = "\x1b[32m" -- Green
statusColor (Just H.Abandoned)  = "\x1b[31m" -- Red
