{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib
  where

import Capabilities.Logging
import Capabilities.Time
import Capabilities.UUID
import Conduit
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
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


type EventUUID = UUID.UUID
type TaskRef = UUID.UUID
type Tasks = Tree.Tree Task
type TaskError = String


data TaskEventType
  = TaskAdd Text.Text
  | TaskStart TaskRef
  | TaskComplete TaskRef
  deriving (Show)


data TaskEvent = TaskEvent
  { eventUuid :: EventUUID
  , timestamp :: Timestamp
  , eventType :: TaskEventType
  } deriving (Show)


data TaskStatus
  = Pending
  | Started
  | Complete
  deriving (Show, Eq)


data Task = Task
  { taskRef :: TaskRef
  , description :: Text.Text
  , createdAt :: Timestamp
  , status :: TaskStatus
  } deriving (Show)


type CanCreateEvent m = (Monad m, CanTime m, CanUuid m)
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

instance (Monad m) => CanStoreEvent (Writer.WriterT [TaskEvent] m) where
  appendEvent x = Writer.tell [x]


type TaskMonad = Writer.WriterT [TaskEvent] (State.StateT Tasks IO)

instance CanUuid TaskMonad where
  uuidGen = lift (lift uuidGen)

instance CanTime TaskMonad where
  now = lift (lift now)

instance CanLog TaskMonad where
  logDebug = undefined
  logWarning = undefined
  logError = undefined


displayTask :: Task -> String
displayTask t
  = if UUID.null (taskRef t)
      then "root"
      else show t


displayTree :: Tasks -> String
displayTree t = show (displayTask <$> t)


readTaskFile :: FilePath -> [TaskEvent]
readTaskFile = undefined


wrapEventType :: (CanCreateEvent m) => TaskEventType -> m TaskEvent
wrapEventType t = TaskEvent <$> uuidGen <*> now <*> pure t


runTaskApi :: TaskMonad a -> IO a
runTaskApi op = do
  State.evalStateT
    (fst <$> Writer.runWriterT op)
    emptyTasks




buildEventTree :: (Foldable f, Traversable f, CanLog m, CanCreateEvent m) => f TaskEventType -> m Tasks
buildEventTree tts
  = mapM wrapEventType tts
  >>= parseEvents emptyTasks


parseEvents :: (Foldable f, CanLog m, CanCreateEvent m) => Tasks -> f TaskEvent -> m Tasks
parseEvents = foldM applyEvent


applyEvent :: (CanLog m, CanCreateEvent m) => Tasks -> TaskEvent -> m Tasks
applyEvent tasks ev@(TaskEvent evId ts evTy)
  = case evTy of
      TaskAdd t ->
        storeTask tasks <$> createTask t

      TaskStart ref -> do
        let t = findTask tasks ref
        case t of
          Nothing -> do
            logWarning "could not find matching task"
            pure tasks
          Just x -> do
            let t' = updateTask x Started
            let ts' = storeTask tasks t'
            pure ts'


applyEventToTasks
  :: (Monad m, CanCreateTask m, HasTasks m)
  => TaskEvent -> m (Either TaskError Task)
applyEventToTasks (TaskEvent evId ts evTy) = do
  tasks <- getTasks
  task <- handleEvent evTy tasks

  case task of
    Left e -> pure (Left e)
    Right v -> do
      let tasks' = storeTask tasks v
      putTasks tasks'
      pure (Right v)

  where
    handleEvent :: (CanCreateTask m) => TaskEventType -> Tasks -> m (Either TaskError Task)
    handleEvent (TaskAdd t) _
      = Right <$> createTask t

    handleEvent (TaskStart ref) tasks
      = case findTask tasks ref of
          Nothing -> do
            pure (Left "could not find matching task")

          Just x -> do
            let t' = updateTask x Started
            pure (Right t')



updateTask :: Task -> TaskStatus -> Task
updateTask t s
  = if UUID.null (taskRef t)
       then t
       else t { status = s }


findTask :: Tasks -> TaskRef -> Maybe Task
findTask ts ref = find matchesRef ts
  where
    matchesRef t = taskRef t == ref



createTask :: (CanCreateTask m) => Text.Text -> m Task
createTask t = mkTask <$> uuidGen <*> now
  where
    mkTask :: TaskRef -> Timestamp -> Task
    mkTask u s = Task u t s Pending


storeTask :: Tasks -> Task -> Tasks
storeTask r@(Tree.Node l ps) t = Tree.Node l (tn : ps)
  where
    tn :: Tasks
    tn = Tree.Node t []


findParent :: Tasks -> Task -> Maybe Task
findParent r t = Nothing


rootTask :: Task
rootTask = Task
  { taskRef = UUID.nil
  , description = "root"
  , createdAt = zeroTime
  , status = Pending
  }


emptyTasks :: Tasks
emptyTasks = Tree.Node rootTask []

