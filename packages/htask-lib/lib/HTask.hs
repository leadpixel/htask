{-# LANGUAGE ConstraintKinds #-}

module HTask
  ( module API
  , module Lib
  , module TaskContainer
  , module Task
  , TaskMonad
  ) where

import Lib
import HTask.API as API
import Event
import HTask.TaskContainer as TaskContainer
import HTask.Task as Task


type TaskMonad m
  = (CanUuid m, CanTime m, CanCreateTask m, CanStoreEvent m)
