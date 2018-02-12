{-# LANGUAGE ConstraintKinds #-}

module HTask.Task
  where

import HTask.Capabilities
import qualified Data.Text as Text
import qualified Data.UUID as UUID


type TaskRef = UUID.UUID


type CanCreateTask m = (Monad m, CanTime m, CanUuid m)


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


createTask :: (CanCreateTask m) => Text.Text -> m Task
createTask t = mkTask <$> uuidGen <*> now
  where
    mkTask :: TaskRef -> Timestamp -> Task
    mkTask u s = Task u t s Pending


setTaskStatus :: TaskStatus -> Task -> Task
setTaskStatus s t = t { status = s }
