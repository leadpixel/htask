{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.Core.TaskEvent
  ( TaskEvent
  , TaskEventDetail (..)
  , TaskIntent (..)
  ) where

import qualified Events          as V
import qualified HTask.Core.Task as H

import           Data.Text       (Text)

import           Data.Aeson
import           GHC.Generics


data TaskIntent
  = AddTask Text
  | StartTask H.TaskUuid
  | StopTask H.TaskUuid
  | CompleteTask H.TaskUuid
  | RemoveTask H.TaskUuid
  deriving (Show, Eq, Generic)

instance ToJSON TaskIntent
instance FromJSON TaskIntent


data TaskEventDetail = TaskEventDetail
  { detailRef :: H.TaskUuid
  , intent    :: TaskIntent
  } deriving (Show, Eq, Generic)

instance ToJSON TaskEventDetail
instance FromJSON TaskEventDetail


type TaskEvent = V.Event TaskEventDetail
