{-# LANGUAGE ConstraintKinds #-}

module HTask
  ( module API
  , module Event
  , module Lib
  , module Capabilities
  , module TaskContainer
  , module Task
  , TaskMonad
  ) where

import Lib
import HTask.API as API
import HTask.Event as Event
import HTask.Capabilities as Capabilities
import HTask.TaskContainer as TaskContainer
import HTask.Task as Task


type TaskMonad m
  = (CanUuid m, CanTime m, CanCreateTask m, CanStoreEvent m)
