{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.Core.TaskEvent
  ( TaskEvent
  , TaskIntent (..)
  ) where

import qualified Data.Aeson       as Aeson
import qualified Leadpixel.Events as V

import           Data.Text        (Text)
import           GHC.Generics
import           HTask.Core.Task


data TaskIntent
  = AddTask TaskUuid Text
  | StartTask TaskUuid
  | StopTask TaskUuid
  | CompleteTask TaskUuid
  | RemoveTask TaskUuid
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON TaskIntent
instance Aeson.FromJSON TaskIntent


type TaskEvent = V.Event TaskIntent
