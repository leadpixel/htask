{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTask.Core.API
  ( AddResult (..)
  , ModifyResult (..)
  , CanAddTask
  , CanModifyTask
  , addTask
  , findTask
  , startTask
  , stopTask
  , completeTask
  , removeTask
  , listTasks
  ) where

import qualified Data.Text                as Text
import qualified Data.Time                as Time
import qualified Leadpixel.Events         as V

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Foldable
import           Data.Sequence            (Seq)
import           Data.Text                (Text)
import           Data.Time                (UTCTime)
import           HTask.Core.Task
import           HTask.Core.TaskContainer
import           HTask.Core.TaskEvent


data AddResult
  = AddSuccess TaskUuid
  | FailedToAdd
  deriving (Show, Eq)


data ModifyResult
  = ModifySuccess Task
  | FailedToModify
  | FailedToFind
  deriving (Show, Eq)


type CanAddTask m = (Monad m, V.HasEventSink m, HasTasks m, CanCreateTask m)
type CanModifyTask m = (Monad m, V.HasEventSink m, HasTasks m)


readTime :: (MonadIO m) => m UTCTime
readTime = liftIO Time.getCurrentTime


addTask
  :: (CanAddTask m, MonadIO m)
  => Text -> m AddResult
addTask tx = do
  tk <- createTask tx
  p <- addNewTask tk

  if p
    then do
      let detail = TaskEventDetail (taskUuid tk) (AddTask tx)
      x <- V.createEvent readTime detail
      V.writeEvent x
      pure $ AddSuccess (taskUuid tk)

    else
      pure FailedToAdd


findTask :: (MonadIO m, HasTasks m) => Text -> m (Maybe Task)
findTask tx
  = find (uuidStartsWith tx) <$> listTasks

  where
    uuidStartsWith :: Text -> Task -> Bool
    uuidStartsWith t
      = Text.isPrefixOf t . taskUuidToText . taskUuid


withMatch :: (MonadIO m, HasTasks m) => Text -> (Task -> m ModifyResult) -> m ModifyResult
withMatch tx op
  = findTask tx >>= maybe (pure FailedToFind) op


startTask
  :: (CanModifyTask m, MonadIO m)
  => Text -> m ModifyResult
startTask tx =
  withMatch tx $ \tsk -> do
    let ref = taskUuid tsk
    p <- updateExistingTask ref $ setTaskStatus InProgress

    case p of
      Nothing ->
        pure FailedToModify

      Just t -> do
        x <- V.createEvent readTime (TaskEventDetail ref (StartTask ref))
        V.writeEvent x
        pure $ ModifySuccess t


stopTask
  :: (CanModifyTask m, MonadIO m)
  => Text -> m ModifyResult
stopTask tx =
  withMatch tx $ \tsk -> do
    let ref = taskUuid tsk
    p <- updateExistingTask ref $ setTaskStatus Pending

    case p of
      Nothing ->
        pure FailedToModify

      Just t -> do
        V.createEvent readTime (TaskEventDetail ref (StopTask ref)) >>= V.writeEvent
        pure $ ModifySuccess t


completeTask
  :: (CanModifyTask m, MonadIO m)
  => Text -> m ModifyResult
completeTask tx =
  withMatch tx $ \tsk -> do
    let ref = taskUuid tsk
    p <- updateExistingTask ref $ setTaskStatus Complete

    case p of
      Nothing ->
        pure FailedToModify

      Just t -> do
        V.createEvent readTime (TaskEventDetail ref (CompleteTask ref)) >>= V.writeEvent
        pure $ ModifySuccess t


removeTask
  :: (CanModifyTask m, MonadIO m)
  => Text -> m ModifyResult
removeTask tx =
  withMatch tx $ \tsk -> do
    let ref = taskUuid tsk
    p <- removeTaskUuid ref

    if p
      then do
        V.createEvent readTime (TaskEventDetail ref (RemoveTask ref)) >>= V.writeEvent
        pure $ ModifySuccess tsk

      else
        pure FailedToModify


listTasks :: (HasTasks m) => m (Seq Task)
listTasks = getTasks
