{-# LANGUAGE ConstraintKinds #-}

module HTask.Core.Task
  ( Task (..)
  , TaskRef
  , TaskStatus (..)
  , CanCreateTask
  , createTask
  , setTaskStatus
  , taskRefText
  ) where

import qualified Data.UUID   as UUID
import qualified Effects     as F

import           Data.Tagged (Tagged (..), untag)
import           Data.Text   (Text)


type TaskIdent = ()
type TaskRef = Tagged TaskIdent UUID.UUID


data TaskStatus
  = Pending
  | InProgress
  | Complete
  | Abandoned
  deriving (Show, Eq)


data Task = Task
  { taskRef     :: TaskRef
  , description :: Text
  , createdAt   :: F.Timestamp
  , status      :: TaskStatus
  } deriving (Show, Eq)


type CanCreateTask m = (Monad m, F.CanTime m, F.CanUuid m)


createTask :: (CanCreateTask m) => Text -> m Task
createTask tex
  = (\u m -> Task u tex m Pending)
  <$> (Tagged <$> F.uuidGen)
  <*> F.now


setTaskStatus :: TaskStatus -> Task -> Task
setTaskStatus s t = t { status = s }


taskRefText :: TaskRef -> Text
taskRefText = UUID.toText . untag
