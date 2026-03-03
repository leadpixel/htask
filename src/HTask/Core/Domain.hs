{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module HTask.Core.Domain
  ( Task (..)
  , TaskEvent
  , TaskIntent (..)
  , TaskMap
  , TaskStatus (..)
  , TaskUuid
  , addNewTask
  , applyEvent
  , createTask
  , disambiguatingPrefixes
  , foldEventLog
  , getTasks
  , removeTaskUuid
  , setTaskStatus
  , taskDisplayOrder
  , taskPriority
  , taskUuidToText
  , updateExistingTask
  ) where

import qualified Control.Monad.State as State
import qualified Data.Aeson          as Aeson
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import qualified Data.Text           as Text
import qualified Data.UUID           as UUID
import qualified HTask.Events        as Events

import           Control.Monad.State (MonadState, runState)
import           Data.Foldable       (foldl')
import           Data.Function       (on)
import           Data.Map.Strict     (Map)
import           Data.Sequence       (Seq, (|>))
import           Data.Tagged         (Tagged (..), untag)
import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import           Data.UUID           (UUID)
import           GHC.Generics        (Generic)
import           HTask.Effects

-- | Task Core Types
type TaskUuid = Tagged "taskId" UUID

data TaskStatus = InProgress | Pending | Complete | Abandoned
  deriving (Eq, Generic, Ord, Show)

instance Aeson.ToJSON TaskStatus
instance Aeson.FromJSON TaskStatus

data Task
  = Task
    { taskUuid    :: TaskUuid
    , description :: Text
    , createdAt   :: UTCTime
    , status      :: TaskStatus
    }
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON Task
instance Aeson.FromJSON Task

createTask :: (MonadTime m, MonadUUID m) => Text -> m Task
createTask tex
  = ((\u m -> Task u tex m Pending) . Tagged <$> nextUUID)
  <*> currentTime

setTaskStatus :: TaskStatus -> Task -> Task
setTaskStatus s t = t { status = s }

taskUuidToText :: TaskUuid -> Text
taskUuidToText = UUID.toText . untag

disambiguatingPrefixes :: [TaskUuid] -> Map TaskUuid Text
disambiguatingPrefixes uuids =
  Map.fromList $ fmap (\u -> (u, findPrefix u)) uuids
  where
    texts = fmap taskUuidToText uuids
    findPrefix u =
      let t = taskUuidToText u
          prefixes = [ Text.take n t
                     | n <- [4..36]
                     , let p = Text.take n t
                     , length (filter (Text.isPrefixOf p) texts) == 1
                     ]
      in case prefixes of
           (p:_) -> p
           []    -> t

taskPriority :: Task -> Task -> Ordering
taskPriority = compare `on` createdAt

taskDisplayOrder :: Task -> Task -> Ordering
taskDisplayOrder a b
  = (compare `on` status) a b <> taskPriority a b

-- | Task Events
data TaskIntent
  = AddTask TaskUuid Text
  | StartTask TaskUuid
  | StopTask TaskUuid
  | CompleteTask TaskUuid
  | RemoveTask TaskUuid
  deriving (Eq, Generic, Show)

instance Aeson.ToJSON TaskIntent
instance Aeson.FromJSON TaskIntent

type TaskEvent = Events.Event TaskIntent

-- | Task Container
type TaskMap = Map TaskUuid Task

getTasks :: (MonadState TaskMap m) => m TaskMap
getTasks = State.get

addNewTask :: (MonadState TaskMap m) => Task -> m Bool
addNewTask t = do
  ts <- State.get
  let (ts', p) = tryInsertTask t ts
  if p
      then do
        State.put ts'
        pure p
      else pure p

  where
    tryInsertTask :: Task -> Map TaskUuid Task -> (Map TaskUuid Task, Bool)
    tryInsertTask t' xs =
        if Map.member (taskUuid t') xs || (description t `List.elem` (description <$> Map.elems xs))
            then (xs, False)
            else (Map.insert (taskUuid t') t' xs, True)

updateExistingTask :: (MonadState TaskMap m) => TaskUuid -> (Task -> Task) -> m (Maybe Task)
updateExistingTask ref op = do
  ts <- State.get
  maybe
    (pure Nothing)
    (\t -> do
      let t' = op t
      State.put $ Map.insert (taskUuid t) t' ts
      pure $ Just t')
    (Map.lookup ref ts)

removeTaskUuid :: (MonadState TaskMap m) => TaskUuid -> m Bool
removeTaskUuid ref = do
  ts <- State.get
  maybe
    (pure False)
    (\_t -> do
      State.put (Map.delete ref ts)
      pure True)
    (Map.lookup ref ts)

-- | Replay Logic
foldEventLog
  :: (Foldable f)
  => f TaskEvent -> (Map TaskUuid Task, Seq TaskEvent)
foldEventLog = foldl' run (mempty, mempty)

  where
    run (accMap, accSeq) x = do
      let (xs, mt) = applyEvent accMap x
      let accSeq' = maybe accSeq (accSeq |>) mt
      (xs, accSeq')

applyEvent :: Map TaskUuid Task -> TaskEvent -> (Map TaskUuid Task, Maybe TaskEvent)
applyEvent xs ev =
  case Events.payload ev of
    (AddTask ref text) -> do
      let t = Task ref text (Events.timestamp ev) Pending
      let (success, xs') = runState (addNewTask t) xs
      if success
         then (xs', Nothing)
         else (xs', Just ev)

    (StartTask ref) -> handleUpdate ref (setTaskStatus InProgress)
    (StopTask ref) -> handleUpdate ref (setTaskStatus Pending)
    (CompleteTask ref) -> handleUpdate ref (setTaskStatus Complete)
    (RemoveTask ref) -> handleUpdate ref (setTaskStatus Abandoned)

  where
    handleUpdate ref op =
      let (xs', success) = tryUpdateTask ref op xs
      in if success then (xs', Nothing) else (xs', Just ev)

tryUpdateTask :: TaskUuid -> (Task -> Task) -> Map TaskUuid Task -> (Map TaskUuid Task, Bool)
tryUpdateTask ref op xs =
  maybe (xs, False)
    (\t -> (Map.insert (taskUuid t) (op t) xs, True))
    (Map.lookup ref xs)
