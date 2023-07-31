{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.Core.TaskEvent
  ( TaskEvent
  , TaskEventDetail (..)
  , TaskIntent (..)
  ) where

import qualified Data.Aeson      as Aeson
import qualified Leadpixel.Events          as V

import           Data.Text       (Text)
import           GHC.Generics
import           HTask.Core.Task


data TaskIntent
  = AddTask Text
  | StartTask TaskUuid
  | StopTask TaskUuid
  | CompleteTask TaskUuid
  | RemoveTask TaskUuid
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON TaskIntent
instance Aeson.FromJSON TaskIntent


data TaskEventDetail = TaskEventDetail
  { detailRef :: TaskUuid
  , intent    :: TaskIntent
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON TaskEventDetail
instance Aeson.FromJSON TaskEventDetail


type TaskEvent = V.Event TaskEventDetail
