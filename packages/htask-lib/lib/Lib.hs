{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib
  where

import HTask.Capabilities
import HTask.Event
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


type TaskRef = UUID.UUID
type Tasks = [Task]
type TaskEvent = Event TaskEventType
type EventLog = [TaskEvent]
type TaskError = String


data TaskEventType
  = TaskAdded Text.Text
  | TaskStarted TaskRef
  | TaskCompleted TaskRef
  | TaskDeleted TaskRef
  deriving (Show)


data TaskStatus
  = Pending
  | InProgress
  | Complete
  | Abandoned
  deriving (Show, Eq)


data Task = Task
  { taskRef :: TaskRef
  , description :: Text.Text
  , createdAt :: Timestamp
  , status :: TaskStatus
  } deriving (Show, Eq)


type CanCreateTask m = (Monad m, CanTime m, CanUuid m)

class HasTasks m where
  getTasks :: m Tasks
  putTasks :: Tasks -> m ()

instance (Monad m) => HasTasks (State.StateT Tasks m) where
  getTasks = State.get
  putTasks = State.put


instance (Monad m, MonadTrans t) => HasTasks (t (State.StateT Tasks m)) where
  getTasks = lift getTasks
  putTasks = lift . putTasks


class CanStoreEvent m where
  appendEvent :: TaskEvent -> m ()

instance (Monad m) => CanStoreEvent (Writer.WriterT EventLog m) where
  appendEvent x = Writer.tell [x]



type TaskMonad m = (CanUuid m, CanTime m, CanCreateTask m, CanStoreEvent m)


wrapEventType :: (CanCreateEvent m) => TaskEventType -> m TaskEvent
wrapEventType t = Event <$> uuidGen <*> now <*> pure t


emptyTasks :: Tasks
emptyTasks = mempty


applyEventToTasks
  :: (Monad m, CanCreateTask m, HasTasks m)
  => TaskEvent -> m (Either TaskError TaskRef)
applyEventToTasks evt = do
  ts <- getTasks

  case eventType evt of
    (TaskAdded t) -> do
      tsk <- createTask t
      putTasks (tsk : ts)
      pure (Right $ taskRef tsk)

    (TaskStarted ref) -> do
      case findTask ts ref of
        Nothing -> do
          pure (Left "could not find matching task")

        Just x -> do
          let tsk = setTaskStatus x InProgress
          let ts' = tsk : (filter (\y -> taskRef x /= taskRef y) ts)
          putTasks ts'
          pure (Right $ taskRef tsk)

    (TaskCompleted ref) -> do
      case findTask ts ref of
        Nothing -> do
          pure (Left "could not find matching task")

        Just x -> do
          let tsk = setTaskStatus x Complete
          let ts' = tsk : (filter (\y -> taskRef x /= taskRef y) ts)
          putTasks ts'
          pure (Right $ taskRef tsk)

    (TaskDeleted ref) -> do
      case findTask ts ref of
        Nothing -> do
          pure (Left "could not find matching task")

        Just x -> do
          let ts' = filter (\y -> taskRef x /= taskRef y) ts
          putTasks ts'
          pure (Right $ taskRef x)




setTaskStatus :: Task -> TaskStatus -> Task
setTaskStatus t s = t { status = s }


findTask :: Tasks -> TaskRef -> Maybe Task
findTask ts ref = find matchesRef ts
  where
    matchesRef t = taskRef t == ref



createTask :: (CanCreateTask m) => Text.Text -> m Task
createTask t = mkTask <$> uuidGen <*> now
  where
    mkTask :: TaskRef -> Timestamp -> Task
    mkTask u s = Task u t s Pending


insertUpdate :: Tasks -> Task -> Tasks
insertUpdate ts t = insertTask t (removeTask (taskRef t) ts)
  where
    removeTask :: TaskRef -> Tasks -> Tasks
    removeTask ref ts = rejectT (\t -> taskRef t /= ref) ts

    insertTask :: Task -> Tasks -> Tasks
    insertTask t ts = t:ts


rejectT :: (Task -> Bool) -> Tasks -> Tasks
rejectT f = filterT (not . f)


filterT :: (Task -> Bool) -> Tasks -> Tasks
filterT = filter


findParent :: Tasks -> Task -> Maybe Task
findParent r t = Nothing
