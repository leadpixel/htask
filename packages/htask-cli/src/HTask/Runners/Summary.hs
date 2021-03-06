{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTask.Runners.Summary
  ( runSummary
  ) where

import qualified Data.Text               as Text
import qualified Data.UUID               as UUID
import qualified HTask.API               as API
import qualified HTask.Task              as H

import           Data.Function
import           Data.List
import           Data.Tagged
import           HTask.Output.Document
import           HTask.Output.Formatters
import           HTask.TaskApplication

import           Data.Semigroup          ((<>))
import           Data.Text               (Text)


taskPriority :: H.Task -> H.Task -> Ordering
taskPriority = compare `on` H.createdAt


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runSummary :: (HasEventBackend m) => m RunResult
runSummary
  = renderSummary <$> runTask API.listTasks


renderSummary :: [H.Task] -> RunResult
renderSummary ts = resultSuccess
  $ displayCurrent ts
  <> ("" : displayTopPending ts)


displayCurrent :: [H.Task] -> [Text]
displayCurrent ts = do
  let ps = filter (hasStatus H.InProgress) ts
  if Data.List.null ps
     then
       [ "No current task" ]
     else
       "Current task:"
       : concatMap printTaskForSummary ps


displayTopPending :: [H.Task] -> [Text]
displayTopPending ts
  = pendingMessage (length xs) (length ps)
  : concatMap printTaskForSummary xs

  where
    ps = sortBy taskPriority (filter (hasStatus H.Pending) ts)

    xs = take 5 ps

    pendingMessage x p =
      "Top " <> tInt x <> " pending (" <> tInt (p - x) <> " hidden):"

    tInt = Text.pack . show


printTaskForSummary :: H.Task -> [Text]
printTaskForSummary t =
  [ indent printDescription
  , indent $ indent printRef
  ]

  where
    printDescription :: Text
    printDescription
      =  statusSymbol (H.status t)
      <> " "
      <> withStatusColor (H.status t) (H.description t)

    printRef :: Text
    printRef = (UUID.toText . untag . H.taskRef) t
