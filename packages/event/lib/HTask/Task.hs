{-# LANGUAGE ConstraintKinds #-}

module HTask.Task
  ( Task (..)
  , TaskRef
  , TaskStatus (..)
  , CanCreateTask
  , createTask
  , setTaskStatus
  ) where

import HTask.Capabilities
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Data.Tagged


type TaskIdent = ()
type TaskRef = Tagged TaskIdent UUID.UUID


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


createTask :: (CanCreateTask m) => Text.Text -> m Task
createTask tex
  = (\u m -> Task u tex m Pending)
  <$> (Tagged <$> uuidGen)
  <*> now


setTaskStatus :: TaskStatus -> Task -> Task
setTaskStatus s t = t { status = s }
