module HTask.Runners.Summary
  ( runSummary
  ) where

import Control.Monad.Reader
import Data.Semigroup ((<>))
import Data.Tagged
import Data.UUID
import HTask.TaskApplication
import qualified Data.Text              as Text
import qualified HTask as H


runSummary :: TaskConfig ()
runSummary
  = do
    ts <- runTask H.listTasks
    lift $ displayCurrent ts
    -- lift $ displayTopPending ts
    -- lift $ displayCompletedCount ts


displayCurrent :: [H.Task] -> IO ()
displayCurrent ts = do
  let ps = filter (isInProgress) ts
  putStrLn "Current task:"
  mapM_ (\t -> putStrLn ("  > " <> showDescription t)) ps
  mapM_ (\t -> putStrLn ("    " <> toString (untag (H.taskRef t)))) ps

    where
      isInProgress = (==) H.InProgress . H.status


showDescription :: H.Task -> String
showDescription t
  =  statusColor (Just $ H.status t)
  <> Text.unpack (H.description t)
  <> statusColor Nothing


statusColor :: Maybe H.TaskStatus -> String
statusColor Nothing             = "\x1b[0m"  -- Clear
statusColor (Just H.Pending)    = "\x1b[34m" -- Blue
statusColor (Just H.InProgress) = "\x1b[33m" -- Yellow
statusColor (Just H.Complete)   = "\x1b[32m" -- Green
statusColor (Just H.Abandoned)  = "\x1b[31m" -- Red
