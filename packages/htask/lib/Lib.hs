{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib
  where

import Conduit
import Control.Monad.IO.Class
import Data.Semigroup
import System.Random
import Data.Foldable
import qualified Control.Monad.Reader as Reader
import Control.Monad
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
  { uuid :: TaskRef
  , description :: Text.Text
  , createdAt :: Timestamp
  , status :: TaskStatus
  } deriving (Show)


displayTask :: Task -> String
displayTask t = if UUID.null (uuid t)
                   then "root"
                   else show t


displayTree :: Tasks -> String
displayTree t = show (displayTask <$> t)


class CanTime m where
  now :: m Timestamp

instance CanTime IO where
  now = Time.systemToUTCTime <$> Time.getSystemTime


class CanUuid m where
  uuidGen :: m TaskRef

instance CanUuid IO where
  uuidGen = UUID.nextRandom




type CanWriteTask m = (Monad m, CanTime m, CanUuid m)



readTaskFile :: FilePath -> [TaskEvent]
readTaskFile = undefined


promote :: (CanWriteTask m) => TaskEventType -> m TaskEvent
promote t = TaskEvent <$> uuidGen <*> now <*> pure t


runEventTree :: Int
runEventTree = undefined


buildEventTree :: (Foldable f, Traversable f, CanWriteTask m) => f TaskEventType -> m Tasks
buildEventTree tts = mapM promote tts >>= parseEvents emptyTree




parseEvents :: (Foldable f, CanWriteTask m) => Tasks -> f TaskEvent -> m Tasks
parseEvents tasks ts = foldM k tasks ts

  where
    k :: (CanWriteTask m) => Tasks -> TaskEvent -> m Tasks
    k tr ev@(TaskEvent evId ts evTy)
      = case evTy of
          TaskAdd t -> storeTask tr <$> createTask t




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
  { uuid = UUID.nil
  , description = "root"
  , createdAt = Time.UTCTime (Time.ModifiedJulianDay 0) (Time.secondsToDiffTime 0)
  , status = Pending
  }


emptyTree :: Tasks
emptyTree = Tree.Node rootTask []

