{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.List
  ( runList
  ) where

import Control.Monad.Reader
import Data.List
import Data.Function
import Data.Semigroup ((<>))
import Data.Tagged
import HTask.Actions
import HTask.TaskApplication
import HTask.Formatters
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


runList :: DetailFlag -> TaskConfig ()
runList d
  = do
    ts <- runTask H.listTasks
    lift $ mapM_
      (mapM_ (nicePrint d))
      (groupBy sameStatus $ sortBy taskDisplayOrder ts)

  where
    sameStatus :: H.Task -> H.Task -> Bool
    sameStatus = (==) `on` H.status


nicePrint :: DetailFlag -> H.Task -> IO ()
nicePrint d t = putStrLn $ Text.unpack
  (  printDetail d
  <> statusSymbol (H.status t)
  <> " "
  <> withStatusColor (H.status t) (H.description t)
  )

  where
    printDetail ShowDetail = UUID.toText (untag (H.taskRef t)) <> " "
    printDetail HideDetail = ""
