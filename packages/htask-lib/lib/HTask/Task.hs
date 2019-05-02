{-# LANGUAGE ConstraintKinds #-}

module HTask.Task
  ( Task (..)
  , TaskRef
  , TaskStatus (..)
  , CanCreateTask
  , createTask
  , setTaskStatus
  , taskRefText
  ) where

import qualified Data.UUID   as UUID
import qualified Event       as V

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
  , createdAt   :: V.Timestamp
  , status      :: TaskStatus
  } deriving (Show, Eq)


type CanCreateTask m = (Monad m, V.CanTime m, V.CanUuid m)


createTask :: (CanCreateTask m) => Text -> m Task
createTask tex
  = (\u m -> Task u tex m Pending)
  <$> (Tagged <$> V.uuidGen)
  <*> V.now


setTaskStatus :: TaskStatus -> Task -> Task
setTaskStatus s t = t { status = s }


taskRefText :: TaskRef -> Text
taskRefText = UUID.toText . untag
