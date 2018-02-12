{-# LANGUAGE ConstraintKinds #-}

module HTask
  ( module API
  , module Lib
  , module Capabilities
  , module TaskContainer
  , module Task
  , TaskMonad
  ) where

import Lib as Lib
import HTask.API as API
import HTask.Capabilities as Capabilities
import HTask.TaskContainer as TaskContainer
import HTask.Task as Task


type TaskMonad m
  = (CanUuid m, CanTime m, CanCreateTask m, CanStoreEvent m)
