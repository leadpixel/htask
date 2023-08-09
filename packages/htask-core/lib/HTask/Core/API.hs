{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module HTask.Core.API
  ( AddResult (..)
  , ModifyResult (..)
  , addTask
  , completeTask
  , findTask
  , listTasks
  , removeTask
  , startTask
  , stopTask
  ) where

import qualified Data.Text                as Text
import qualified Leadpixel.Events         as V

import           Data.Foldable
import           Data.Sequence            (Seq)
import           Data.Text                (Text)
import           Data.Time                (UTCTime)
import           Data.UUID                (UUID)
import           HTask.Core.Task
import           HTask.Core.TaskContainer
import           HTask.Core.TaskEvent
import           Leadpixel.Provider


data AddResult
  = AddSuccess TaskUuid
  | FailedToAdd
  deriving (Eq, Show)


data ModifyResult
  = ModifySuccess Task
  | FailedToModify
  | FailedToFind
  deriving (Eq, Show)


type CanAddTask m = (Monad m, V.HasEventSink m, HasTasks m, Provider UUID m, Provider UTCTime m)
type CanModifyTask m = (Monad m, Provider UTCTime m, V.HasEventSink m, HasTasks m)


listTasks :: (HasTasks m) => m (Seq Task)
listTasks = getTasks


findTask :: (HasTasks m, Monad m) => Text -> m (Maybe Task)
findTask tx
  = find (uuidStartsWith tx) <$> getTasks

  where
    uuidStartsWith :: Text -> Task -> Bool
    uuidStartsWith t
      = Text.isPrefixOf t . taskUuidToText . taskUuid


withMatch :: (HasTasks m, Monad m) => Text -> (Task -> m ModifyResult) -> m ModifyResult
withMatch tx op
  = findTask tx >>= maybe (pure FailedToFind) op


addTask
  :: (CanAddTask m)
  => Text -> m AddResult
addTask tx = do
  tk <- createTask tx
  p <- addNewTask tk

  if p
    then do
      let detail = TaskEventDetail (taskUuid tk) (AddTask tx)
      V.createEvent detail >>= V.writeEvent
      pure $ AddSuccess (taskUuid tk)

    else
      pure FailedToAdd


startTask :: (CanModifyTask m) => Text -> m ModifyResult
startTask tx =
  withMatch tx $ \tsk -> do
    let ref = taskUuid tsk
    p <- updateExistingTask ref $ setTaskStatus InProgress

    case p of
      Nothing ->
        pure FailedToModify

      Just t -> do
        V.createEvent (TaskEventDetail ref (StartTask ref)) >>= V.writeEvent
        pure $ ModifySuccess t


stopTask :: (CanModifyTask m) => Text -> m ModifyResult
stopTask tx =
  withMatch tx $ \tsk -> do
    let ref = taskUuid tsk
    p <- updateExistingTask ref $ setTaskStatus Pending

    case p of
      Nothing ->
        pure FailedToModify

      Just t -> do
        V.createEvent (TaskEventDetail ref (StopTask ref)) >>= V.writeEvent
        pure $ ModifySuccess t


completeTask :: (CanModifyTask m) => Text -> m ModifyResult
completeTask tx =
  withMatch tx $ \tsk -> do
    let ref = taskUuid tsk
    p <- updateExistingTask ref $ setTaskStatus Complete

    case p of
      Nothing ->
        pure FailedToModify

      Just t -> do
        V.createEvent (TaskEventDetail ref (CompleteTask ref)) >>= V.writeEvent
        pure $ ModifySuccess t


removeTask :: (CanModifyTask m) => Text -> m ModifyResult
removeTask tx =
  withMatch tx $ \tsk -> do
    let ref = taskUuid tsk
    p <- removeTaskUuid ref

    if p
      then do
        V.createEvent (TaskEventDetail ref (RemoveTask ref)) >>= V.writeEvent
        pure $ ModifySuccess tsk

      else
        pure FailedToModify
