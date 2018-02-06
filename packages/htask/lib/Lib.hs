{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib
  where

import Conduit
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Monoid
import Data.Semigroup
import Data.Time
import Data.Tree
import System.Random
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import qualified Data.Tree as Tree
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID


type EventUUID = UUID.UUID
type TaskRef = UUID.UUID
type Timestamp = Time.UTCTime
type Tasks = Tree.Tree Task


data TaskEventType
  = TaskAdd Text.Text
  | TaskStart TaskRef
  | TaskComplete TaskRef
  | TaskAbandon
  | TaskAssign
  deriving (Show)


data TaskEvent = TaskEvent
  { eventUuid :: EventUUID
  , timestamp :: Timestamp
  , eventType :: TaskEventType
  } deriving (Show)


data TaskStatus
  = Pending
  | Started
  | Blocked
  | Complete
  | Aborted
  deriving (Show, Eq)


data Task = Task
  { taskRef :: TaskRef
  , description :: Text.Text
  , createdAt :: Timestamp
  , status :: TaskStatus
  } deriving (Show)


class CanLog m where
  logDebug :: (Show a) => a -> m ()
  logWarning :: (Show a) => a -> m ()
  logError :: (Show a) => a -> m ()

instance CanLog IO where
  logDebug = print . show
  logWarning = print . show
  logError = print . show

instance CanLog (State.StateT a IO) where
  logDebug = liftIO . logDebug
  logWarning = liftIO . logWarning
  logError = liftIO . logError



class HasTasks m where
  getTasks :: m Tasks
  putTasks :: Tasks -> m ()

instance (Monad m) => HasTasks (State.StateT Tasks m) where
  getTasks = State.get
  putTasks = State.put


class CanTime m where
  now :: m Timestamp

instance CanTime IO where
  now = Time.systemToUTCTime <$> Time.getSystemTime

instance CanTime (State.StateT a IO) where
  now = liftIO now


class CanUuid m where
  uuidGen :: m TaskRef

instance CanUuid IO where
  uuidGen = UUID.nextRandom

instance CanUuid (State.StateT a IO) where
  uuidGen = liftIO uuidGen




type CanWriteTask m = (Monad m, CanTime m, CanUuid m)



displayTask :: Task -> String
displayTask t = if UUID.null (taskRef t)
                   then "root"
                   else show t


displayTree :: Tasks -> String
displayTree t = show (displayTask <$> t)


readTaskFile :: FilePath -> [TaskEvent]
readTaskFile = undefined


promote :: (CanWriteTask m) => TaskEventType -> m TaskEvent
promote t = TaskEvent <$> uuidGen <*> now <*> pure t


runEventTree :: State.StateT Tasks IO a -> IO a
runEventTree op = do
  State.evalStateT op emptyTasks




buildEventTree :: (Foldable f, Traversable f, CanLog m, CanWriteTask m) => f TaskEventType -> m Tasks
buildEventTree tts = mapM promote tts >>= parseEvents emptyTasks




parseEvents :: (Foldable f, CanLog m, CanWriteTask m) => Tasks -> f TaskEvent -> m Tasks
parseEvents tasks ts = foldM applyEvent tasks ts


applyEvent :: (CanLog m, CanWriteTask m) => Tasks -> TaskEvent -> m Tasks
applyEvent tasks ev@(TaskEvent evId ts evTy)
  = case evTy of
      TaskAdd t -> storeTask tasks <$> createTask t
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


updateTask :: Task -> TaskStatus -> Task
updateTask t s = case UUID.null (taskRef t) of
                   True -> t
                   False -> t { status = s }


findTask :: Tasks -> TaskRef -> Maybe Task
findTask ts ref = find matchesRef ts
  where
    matchesRef t = taskRef t == ref



createTask :: (CanWriteTask m) => Text.Text -> m Task
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
  , createdAt = Time.UTCTime (Time.ModifiedJulianDay 0) (Time.secondsToDiffTime 0)
  , status = Pending
  }


emptyTasks :: Tasks
emptyTasks = Tree.Node rootTask []

