{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib
  ( TaskEventType (..)
  , buildEventTree
  , displayTree
  ) where

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


type TaskUUID = UUID.UUID
type Timestamp = Time.UTCTime
type TaskTree = Tree.Tree Task


data TaskEventType
  = TaskAdd Text.Text
  | TaskComplete
  | TaskAbandon
  | TaskAssign
  deriving (Show)


data TaskEvent = TaskEvent TaskUUID Timestamp TaskEventType
  deriving (Show)


data TaskStatus
  = Pending
  | Started
  | Blocked
  | Complete
  | Aborted
  deriving (Show, Eq)


data Task = Task
  { uuid :: TaskUUID
  , description :: Text.Text
  , createdAt :: Timestamp
  , status :: TaskStatus
  } deriving (Show)


displayTask :: Task -> String
displayTask t = if UUID.null (uuid t)
                   then "root"
                   else show t


displayTree :: TaskTree -> String
displayTree t = show (displayTask <$> t)


class CanTime m where
  now :: m Timestamp

instance CanTime IO where
  now = Time.systemToUTCTime <$> Time.getSystemTime


class CanUuid m where
  uuidGen :: m TaskUUID

instance CanUuid IO where
  uuidGen = UUID.nextRandom




type CanCreateTask m = (Monad m, CanTime m, CanUuid m)



readTaskFile :: FilePath -> [TaskEvent]
readTaskFile = undefined


buildEventTree :: (Foldable f, Traversable f, CanCreateTask m) => f TaskEventType -> m TaskTree
buildEventTree tts = mapM k tts >>= parseEvents
  where
    k :: (CanCreateTask m) => TaskEventType -> m TaskEvent
    k t = TaskEvent <$> uuidGen <*> now <*> pure t


parseEvents :: (Foldable f, CanCreateTask m) => f TaskEvent -> m TaskTree
parseEvents ts = foldM k emptyTree ts

  where
    k :: (CanCreateTask m) => TaskTree -> TaskEvent -> m TaskTree
    k tr ev@(TaskEvent evId ts evTy)
      = case evTy of
          TaskAdd t -> addTask tr <$> createTask t




createTask :: (CanCreateTask m) => Text.Text -> m Task
createTask t = mkTask <$> uuidGen <*> now
  where
    mkTask :: TaskUUID -> Timestamp -> Task
    mkTask u s = Task u t s Pending


addTask :: TaskTree -> Task -> TaskTree
addTask r@(Tree.Node l ps) t = Tree.Node l (tn : ps)
  where
    tn :: TaskTree
    tn = Tree.Node t []


findParent :: TaskTree -> Task -> Maybe Task
findParent r t = Nothing


rootTask :: Task
rootTask = Task
  { uuid = UUID.nil
  , description = "root"
  , createdAt = Time.UTCTime (Time.ModifiedJulianDay 0) (Time.secondsToDiffTime 0)
  , status = Pending
  }


emptyTree :: TaskTree
emptyTree = Tree.Node rootTask []

