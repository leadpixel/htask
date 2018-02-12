{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib
  where

import HTask.Capabilities
import HTask.Event
import HTask.TaskContainer
import HTask.Task
import Conduit
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Semigroup
import Data.Tree
import System.Random
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.State as State
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Tree as Tree
import qualified Data.UUID as UUID


data TaskEventDetail = TaskEventDetail
  { detailRef :: TaskRef
  , intent :: TaskIntent
  } deriving (Show)


type TaskEvent = Event TaskEventDetail
type EventLog = [TaskEvent]
type TaskError = String


data TaskIntent
  = AddTask Text.Text
  | StartTask TaskRef
  | CompleteTask TaskRef
  | DeleteTask TaskRef
  deriving (Show)


class CanStoreEvent m where
  appendEvent :: TaskEvent -> m ()

instance (Monad m) => CanStoreEvent (Writer.WriterT EventLog m) where
  appendEvent x = Writer.tell [x]


wrapEventType :: (CanCreateEvent m) => TaskEventDetail -> m TaskEvent
wrapEventType t = Event <$> uuidGen <*> now <*> pure t


applyIntentToTasks
  :: (Monad m, CanCreateTask m, HasTasks m)
  => TaskIntent -> m (Either TaskError TaskEventDetail)
applyIntentToTasks intent =
  case intent of
    (AddTask text) -> do
      t <- createTask text
      p <- addNewTask t
      pure $ if p
        then Right (TaskEventDetail (taskRef t) intent)
        else Left "could not add; non-unique id"

    (StartTask ref) -> do
      p <- updateExistingTask ref $ setTaskStatus InProgress
      pure $ if p
        then Right (TaskEventDetail ref intent)
        else Left "could not find matching id"

    (CompleteTask ref) -> do
      p <- updateExistingTask ref $ setTaskStatus Complete
      pure $ if p
        then Right (TaskEventDetail ref intent)
        else Left "could not find matching id"

    (DeleteTask ref) -> do
      p <- removeTask ref
      pure $ if p
        then Right (TaskEventDetail ref intent)
        else Left "unknown fuckup"

