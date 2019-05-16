{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.TaskEvent
  ( TaskEvent
  , TaskEventDetail (..)
  , TaskIntent (..)
  ) where

import qualified Events       as V
import qualified HTask.Task   as H

import           Data.Text    (Text)

import           Data.Aeson
import           GHC.Generics


data TaskIntent
  = AddTask Text
  | StartTask H.TaskRef
  | StopTask H.TaskRef
  | CompleteTask H.TaskRef
  | RemoveTask H.TaskRef
  deriving (Show, Eq, Generic)

instance ToJSON TaskIntent
instance FromJSON TaskIntent


data TaskEventDetail = TaskEventDetail
  { detailRef :: H.TaskRef
  , intent    :: TaskIntent
  } deriving (Show, Eq, Generic)

instance ToJSON TaskEventDetail
instance FromJSON TaskEventDetail


type TaskEvent = V.Event TaskEventDetail
