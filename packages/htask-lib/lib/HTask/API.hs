{-# LANGUAGE ConstraintKinds #-}

module HTask.API
  ( AddResult (..)
  , ModifyResult (..)
  , CanAddTask
  , CanModifyTask
  , addTask
  , startTask
  , stopTask
  , completeTask
  , removeTask
  , listTasks
  ) where

import qualified Data.Text           as Text
import qualified Events              as V
import qualified HTask.Task          as H
import qualified HTask.TaskContainer as HC
import qualified HTask.TaskEvent     as TV

import           Data.Functor        (($>))
import           Data.Text           (Text)


data AddResult
  = AddSuccess H.TaskRef
  | FailedToAdd
  deriving (Show, Eq)


data ModifyResult
  = ModifySuccess H.Task
  | FailedToModify
  | FailedToFind
  deriving (Show, Eq)


type CanAddTask m = (Monad m, V.HasEventSink m, V.CanCreateEvent m, HC.HasTasks m, H.CanCreateTask m)
type CanModifyTask m = (Monad m, V.HasEventSink m, V.CanCreateEvent m, HC.HasTasks m)


addTask
  :: (CanAddTask m)
  => Text -> m AddResult
addTask tx = do
  tk <- H.createTask tx
  p <- HC.addNewTask tk

  if p
    then
      (V.createEvent (TV.TaskEventDetail (H.taskRef tk) (TV.AddTask tx)) >>= V.writeEvent) $> AddSuccess (H.taskRef tk)

    else
      pure FailedToAdd



headSafe :: [a] -> Maybe a
headSafe []    = Nothing
headSafe (x:_) = Just x


findMatch :: (Monad m, HC.HasTasks m) => Text -> m (Maybe H.Task)
findMatch ref
  = headSafe . filterMatchesUUID ref <$> listTasks

 where
   filterMatchesUUID :: Text -> [H.Task] -> [H.Task]
   filterMatchesUUID t
     = filter (Text.isPrefixOf t . H.taskRefText . H.taskRef)


withMatch :: (Monad m, HC.HasTasks m) => Text -> (H.Task -> m ModifyResult) -> m ModifyResult
withMatch tx op
  = findMatch tx >>= maybe (pure FailedToFind) op


startTask
  :: (CanModifyTask m)
  => Text -> m ModifyResult
startTask tx =
  withMatch tx $ \tsk -> do
    let ref = H.taskRef tsk
    p <- HC.updateExistingTask ref $ H.setTaskStatus H.InProgress

    case p of
      Just t -> do
        V.createEvent (TV.TaskEventDetail ref (TV.StartTask ref)) >>= V.writeEvent
        pure $ ModifySuccess t

      Nothing ->
        pure FailedToModify


stopTask
  :: (CanModifyTask m)
  => Text -> m ModifyResult
stopTask tx =
  withMatch tx $ \tsk -> do
    let ref = H.taskRef tsk
    p <- HC.updateExistingTask ref $ H.setTaskStatus H.Pending

    case p of
      Just t -> do
        V.createEvent (TV.TaskEventDetail ref (TV.StopTask ref)) >>= V.writeEvent
        pure $ ModifySuccess t

      Nothing ->
        pure FailedToModify


completeTask
  :: (CanModifyTask m)
  => Text -> m ModifyResult
completeTask tx =
  withMatch tx $ \tsk -> do
    let ref = H.taskRef tsk
    p <- HC.updateExistingTask ref $ H.setTaskStatus H.Complete

    case p of
      Just t -> do
        V.createEvent (TV.TaskEventDetail ref (TV.CompleteTask ref)) >>= V.writeEvent
        pure $ ModifySuccess t

      Nothing ->
        pure FailedToModify


removeTask
  :: (CanModifyTask m)
  => Text -> m ModifyResult
removeTask tx =
  withMatch tx $ \tsk -> do
    let ref = H.taskRef tsk
    p <- HC.removeTaskRef ref

    if p
      then do
        V.createEvent (TV.TaskEventDetail ref (TV.RemoveTask ref)) >>= V.writeEvent
        pure $ ModifySuccess tsk

      else
        pure FailedToModify


listTasks :: (HC.HasTasks m) => m HC.Tasks
listTasks = HC.getTasks

