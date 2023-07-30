{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module HTask.Core.Task
  ( Task (..)
  , TaskUuid
  , TaskStatus (..)
  , CanCreateTask
  , createTask
  , setTaskStatus
  , taskUuidToText
  ) where

import qualified Data.UUID          as UUID

import           Data.Tagged        (Tagged (..), untag)
import           Data.Text          (Text)
import           Data.Time          (UTCTime)
import           Data.UUID          (UUID)
import           GHC.Generics
import           Leadpixel.Provider


type TaskIdent = ()
type TaskUuid = Tagged TaskIdent UUID


data TaskStatus
  = Pending
  | InProgress
  | Complete
  | Abandoned
  deriving (Show, Eq, Generic)


data Task = Task
  { taskUuid    :: TaskUuid
  , description :: Text
  , createdAt   :: UTCTime
  , status      :: TaskStatus
  } deriving (Show, Eq, Generic)


type CanCreateTask m = (Monad m, Provider UTCTime m, Provider UUID m)


createTask :: (CanCreateTask m) => Text -> m Task
createTask tex
  = (\u m -> Task u tex m Pending)
  <$> (Tagged <$> provide @UUID)
  <*> provide @UTCTime


setTaskStatus :: TaskStatus -> Task -> Task
setTaskStatus s t = t { status = s }


taskUuidToText :: TaskUuid -> Text
taskUuidToText = UUID.toText . untag