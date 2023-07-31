{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTask.CLI.Runners.List
  ( runList
  ) where

import qualified Data.Sequence               as Seq
import qualified Data.UUID                   as UUID
import qualified HTask.Core                  as H
import qualified Leadpixel.Events            as V

import           Control.Monad.IO.Class      (MonadIO)
import           Data.Foldable
import           Data.Function
import           Data.Sequence               (Seq)
import           Data.Tagged
import           Data.Text                   (Text)
import           HTask.CLI.Output.Document
import           HTask.CLI.Output.Formatters
import           HTask.CLI.TaskApplication


taskDisplayOrder :: H.Task -> H.Task -> Ordering
taskDisplayOrder a b
  = byStatus a b <> byTimestamp a b

  where
    byStatus
      = statusDisplayOrder `on` H.status

    byTimestamp
      = compare `on` H.createdAt


-- TODO: replace with common sense / enum instance
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


-- TODO: replace bool args with Tagged "showUuid"
runList :: (MonadIO m, V.HasEventSource m) => Bool -> Bool -> m RunResult
runList showUUID showAll
  = resultSuccess . toList . fmap formatOutput . selectTasks
  <$> runTask H.listTasks

  where
    formatOutput
      = nicePrint showUUID

    selectTasks :: Seq H.Task -> Seq H.Task
    selectTasks
      = Seq.sortBy taskDisplayOrder
      . (if showAll
            then id
            else filterActive
        )

    filterActive
      = Seq.filter (justActive . H.status)

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
    printUUID = UUID.toText (untag (H.taskUuid t)) <> " "
