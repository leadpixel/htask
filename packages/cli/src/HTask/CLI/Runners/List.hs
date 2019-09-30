{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTask.CLI.Runners.List
  ( runList
  ) where

import qualified Data.UUID                   as UUID
import qualified Events                      as V
import qualified HTask.Core.API              as API
import qualified HTask.Core.Task             as H

import           Data.Function
import           Data.List
import           Data.Tagged
import           HTask.CLI.Output.Document
import           HTask.CLI.Output.Formatters
import           HTask.CLI.TaskApplication

import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)


taskDisplayOrder :: H.Task -> H.Task -> Ordering
taskDisplayOrder a b
  = byStatus <> byTimestamp

  where
    byStatus
      = on statusDisplayOrder H.status a b

    byTimestamp
      = on compare H.createdAt a b


statusDisplayOrder :: H.TaskStatus -> H.TaskStatus -> Ordering
statusDisplayOrder  H.InProgress  H.Abandoned  =  LT
statusDisplayOrder  H.InProgress  H.Complete   =  LT
statusDisplayOrder  H.InProgress  H.Pending    =  LT
statusDisplayOrder  H.InProgress  H.InProgress =  EQ

statusDisplayOrder  H.Pending     H.Abandoned  =  LT
statusDisplayOrder  H.Pending     H.Complete   =  LT
statusDisplayOrder  H.Pending     H.Pending    =  EQ
statusDisplayOrder  H.Pending     H.InProgress =  GT

statusDisplayOrder  H.Complete    H.Abandoned  =  LT
statusDisplayOrder  H.Complete    H.Complete   =  EQ
statusDisplayOrder  H.Complete    H.InProgress =  GT
statusDisplayOrder  H.Complete    H.Pending    =  GT

statusDisplayOrder  H.Abandoned   H.Abandoned  =  EQ
statusDisplayOrder  H.Abandoned   H.Complete   =  GT
statusDisplayOrder  H.Abandoned   H.InProgress =  GT
statusDisplayOrder  H.Abandoned   H.Pending    =  GT


runList :: (Monad m, V.HasEventSource m) => Bool -> Bool -> m RunResult
runList showUUID showAll
  = resultSuccess . fmap formatOutput . selectTasks
  <$> runTask API.listTasks

  where
    formatOutput
      = nicePrint showUUID

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


nicePrint :: Bool -> H.Task -> Text
nicePrint d t
  =  ( if d then printUUID else "" )
  <> statusSymbol (H.status t)
  <> " "
  <> withStatusColor (H.status t) (H.description t)

  where
    printUUID = UUID.toText (untag (H.taskRef t)) <> " "
