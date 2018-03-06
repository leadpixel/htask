{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.Runners.List
  ( runList
  ) where

import Data.List
import Data.Function
import Data.Semigroup ((<>))
import Data.Tagged
import HTask.Actions
import HTask.Output
import HTask.TaskApplication
import qualified Data.Text              as Text
import qualified Data.UUID              as UUID
import qualified HTask as H


taskDisplayOrder :: H.Task -> H.Task -> Ordering
taskDisplayOrder = statusDisplayOrder `on` H.status


statusDisplayOrder :: H.TaskStatus -> H.TaskStatus -> Ordering
statusDisplayOrder  H.InProgress  H.InProgress  =  EQ
statusDisplayOrder  H.Pending     H.InProgress  =  GT
statusDisplayOrder  H.Complete    H.InProgress  =  GT
statusDisplayOrder  H.Abandoned   H.InProgress  =  GT

statusDisplayOrder  H.InProgress  H.Pending     =  LT
statusDisplayOrder  H.Pending     H.Pending     =  EQ
statusDisplayOrder  H.Complete    H.Pending     =  GT
statusDisplayOrder  H.Abandoned   H.Pending     =  GT

statusDisplayOrder  H.InProgress  H.Complete    =  LT
statusDisplayOrder  H.Pending     H.Complete    =  LT
statusDisplayOrder  H.Complete    H.Complete    =  EQ
statusDisplayOrder  H.Abandoned   H.Complete    =  GT

statusDisplayOrder  H.InProgress  H.Abandoned   =  LT
statusDisplayOrder  H.Pending     H.Abandoned   =  LT
statusDisplayOrder  H.Complete    H.Abandoned   =  LT
statusDisplayOrder  H.Abandoned   H.Abandoned   =  EQ


runList :: ShowUUID -> IncludeDeleted -> TaskConfig Document
runList showUUID showDeleted = do
  ts <- runTask H.listTasks
  pure $ Document $ fmap formatOutput (selectTasks ts)

  where
    -- formatOutput :: H.Task -> Block
    formatOutput = line . nicePrint showUUID

    selectTasks :: [H.Task] -> [H.Task]
    selectTasks
      = sortBy taskDisplayOrder
      . (if showDeleted
            then id
            else filter notAbandoned
        )

    notAbandoned :: H.Task -> Bool
    notAbandoned t = H.status t /= H.Abandoned


nicePrint :: ShowUUID -> H.Task -> Text.Text
nicePrint d t
  =  ( if d then printUUID else "" )
  <> statusSymbol (H.status t)
  <> " "
  <> withStatusColor (H.status t) (H.description t)

  where
    printUUID = UUID.toText (untag (H.taskRef t)) <> " "
