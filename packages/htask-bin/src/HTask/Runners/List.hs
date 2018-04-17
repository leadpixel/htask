{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Runners.List
  ( runList
  ) where

import qualified Data.Text              as Text
import qualified Data.UUID              as UUID
import qualified HTask                  as H

import Data.List
import Data.Function
import Data.Tagged
import HTask.Actions
import HTask.Output
import HTask.TaskApplication

import Data.Semigroup ((<>))


taskDisplayOrder :: H.Task -> H.Task -> Ordering
taskDisplayOrder a b
  = byStatus <> byTimestamp

  where
    byStatus
      = on statusDisplayOrder H.status a b

    byTimestamp
      = on compare H.createdAt a b


statusDisplayOrder :: H.TaskStatus -> H.TaskStatus -> Ordering
statusDisplayOrder  H.InProgress  H.Abandoned   =  LT
statusDisplayOrder  H.InProgress  H.Complete    =  LT
statusDisplayOrder  H.InProgress  H.Pending     =  LT
statusDisplayOrder  H.InProgress  H.InProgress  =  EQ

statusDisplayOrder  H.Pending     H.Abandoned   =  LT
statusDisplayOrder  H.Pending     H.Complete    =  LT
statusDisplayOrder  H.Pending     H.Pending     =  EQ
statusDisplayOrder  H.Pending     H.InProgress  =  GT

statusDisplayOrder  H.Complete    H.Abandoned   =  LT
statusDisplayOrder  H.Complete    H.Complete    =  EQ
statusDisplayOrder  H.Complete    H.InProgress  =  GT
statusDisplayOrder  H.Complete    H.Pending     =  GT

statusDisplayOrder  H.Abandoned   H.Abandoned   =  EQ
statusDisplayOrder  H.Abandoned   H.Complete    =  GT
statusDisplayOrder  H.Abandoned   H.InProgress  =  GT
statusDisplayOrder  H.Abandoned   H.Pending     =  GT


runList :: (HasEventBackend m) => ShowUUID -> ShowAll -> m Document
runList showUUID showAll
  =   Document . fmap formatOutput . selectTasks
  <$> runTask H.listTasks

  where
    formatOutput
      = line . nicePrint showUUID

    selectTasks
      = sortBy taskDisplayOrder
      . (if showAll
            then id
            else filterActive
        )

    filterActive
      = filter (justActive . H.status)

    justActive H.Pending    = True
    justActive H.InProgress = True
    justActive H.Complete   = False
    justActive H.Abandoned  = False


nicePrint :: ShowUUID -> H.Task -> Text.Text
nicePrint d t
  =  ( if d then printUUID else "" )
  <> statusSymbol (H.status t)
  <> " "
  <> withStatusColor (H.status t) (H.description t)

  where
    printUUID = UUID.toText (untag (H.taskRef t)) <> " "
