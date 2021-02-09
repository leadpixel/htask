{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Core.Task
  ( Task (..)
  , TaskRef
  , TaskStatus (..)
  , CanCreateTask
  , createTask
  , setTaskStatus
  , taskRefText
  ) where

import qualified Data.UUID          as UUID

import           Data.Tagged        (Tagged (..), untag)
import           Data.Text          (Text)
import           Data.Time          (UTCTime)
import           Data.UUID          (UUID)
import           GHC.Generics
import           Leadpixel.Provider


type TaskIdent = ()
type TaskRef = Tagged TaskIdent UUID


data TaskStatus
  = Pending
  | InProgress
  | Complete
  | Abandoned
  deriving (Show, Eq, Generic)


data Task = Task
  { taskRef     :: TaskRef
  , description :: Text
  , createdAt   :: UTCTime
  , status      :: TaskStatus
  } deriving (Show, Eq, Generic)


type CanCreateTask m = (Monad m, Provider UTCTime m, Provider UUID m)


createTask :: (CanCreateTask m) => Text -> m Task
createTask tex
  = (\u m -> Task u tex m Pending)
  <$> (Tagged <$> provide)
  <*> provide


setTaskStatus :: TaskStatus -> Task -> Task
setTaskStatus s t = t { status = s }


taskRefText :: TaskRef -> Text
taskRefText = UUID.toText . untag
