{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.Summary
  ( runSummary
  ) where

import qualified Data.Text    as Text
import qualified Data.UUID    as UUID
import qualified HTask        as H

import Control.Monad.IO.Class
import Data.Function
import Data.List
import Data.Tagged
import Event
import HTask.Output
import HTask.TaskApplication

import Data.Semigroup ((<>))


taskPriority :: H.Task -> H.Task -> Ordering
taskPriority = compare `on` H.createdAt


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


runSummary :: (HasEventBackend m) => m Document
runSummary
  = renderSummary <$> runTask H.listTasks


renderSummary :: [H.Task] -> Document
renderSummary ts = Document
  $ displayCurrent ts
  <> (line "" : displayTopPending ts)


displayCurrent :: [H.Task] -> [Block]
displayCurrent ts = do
  let ps = filter (hasStatus H.InProgress) ts
  if Data.List.null ps
     then
       [ line "No current task" ]
     else
       line "Current task:"
       : concatMap printTaskForSummary ps


displayTopPending :: [H.Task] -> [Block]
displayTopPending ts
  = pendingMessage (length xs) (length ps)
  : concatMap printTaskForSummary xs

  where
    ps = sortBy taskPriority (filter (hasStatus H.Pending) ts)

    xs = take 5 ps

    pendingMessage x p = line
      ( "Top " <> tInt x <> " pending (" <> tInt (p - x) <> " hidden):" )

    tInt = Text.pack . show


printTaskForSummary :: H.Task -> [Block]
printTaskForSummary t =
  [ line (indent printDescription)
  , line (indent $ indent printRef)
  ]

  where
    printDescription :: Text.Text
    printDescription
      =  statusSymbol (H.status t)
      <> " "
      <> withStatusColor (H.status t) (H.description t)

    printRef :: Text.Text
    printRef = (UUID.toText . untag . H.taskRef) t
