{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import qualified Data.Foldable       as Foldable
import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Maybe          as Maybe
import qualified Data.Text           as Text
import qualified HTask.Events        as V
import qualified Text.Read           as Text

import           Control.Monad.State (MonadState)
import           Data.Text           (Text)
import           HTask.Core.Domain
import           HTask.Effects


data AddResult
  = AddSuccess TaskUuid
  | FailedToAdd
  | EmptyDescription
  deriving (Eq, Show)


data ModifyResult
  = ModifySuccess Task
  | FailedToModify
  | FailedToFind
  deriving (Eq, Show)


type CanModifyTask m = (MonadTime m, V.HasEventSink m, MonadState TaskMap m)


listTasks :: (MonadState TaskMap m) => m [Task]
listTasks = Map.elems <$> getTasks


findTask :: (MonadState TaskMap m) => Text -> m (Maybe Task)
findTask tx = do
  tasks <- getTasks
  let sorted = List.sortBy taskDisplayOrder (Map.elems tasks)
  pure $ case Text.readMaybe (Text.unpack tx) of
    Just (n :: Int) | n > 0 -> Maybe.listToMaybe $ drop (n - 1) sorted
    _                       -> Foldable.find (uuidStartsWith tx) sorted

  where
    uuidStartsWith :: Text -> Task -> Bool
    uuidStartsWith t
      = Text.isPrefixOf t . taskUuidToText . taskUuid


withMatch :: (MonadState TaskMap m) => Text -> (Task -> m ModifyResult) -> m ModifyResult
withMatch tx op
  = findTask tx >>= maybe (pure FailedToFind) op


addTask
  :: (MonadTime m, MonadUUID m, MonadState TaskMap m, V.HasEventSink m)
  => Text -> m AddResult
addTask tx
  | Text.null (Text.strip tx) = pure EmptyDescription
  | otherwise = do
      tk <- createTask tx
      addResult <- addNewTask tk

      let intent = AddTask (taskUuid tk) tx
      (ev :: TaskEvent) <- V.createEvent intent

      if addResult
        then do
          V.writeEvent ev
          pure $ AddSuccess (taskUuid tk)

        else
          pure FailedToAdd


startTask :: (CanModifyTask m) => Text -> m ModifyResult
startTask tx =
  withMatch tx $ \tsk ->
    case status tsk of
      Pending -> do
        let ref = taskUuid tsk
        updateResult <- updateExistingTask ref $ setTaskStatus InProgress

        case updateResult of
          Nothing ->
            pure FailedToModify

          Just t -> do
            V.createEvent (StartTask ref) >>= V.writeEvent
            pure $ ModifySuccess t

      _ -> pure FailedToModify


stopTask :: (CanModifyTask m) => Text -> m ModifyResult
stopTask tx =
  withMatch tx $ \tsk ->
    case status tsk of
      InProgress -> do
        let ref = taskUuid tsk
        updateResult <- updateExistingTask ref $ setTaskStatus Pending

        case updateResult of
          Nothing ->
            pure FailedToModify

          Just t -> do
            V.createEvent (StopTask ref) >>= V.writeEvent
            pure $ ModifySuccess t

      _ -> pure FailedToModify


completeTask :: (CanModifyTask m) => Text -> m ModifyResult
completeTask tx =
  withMatch tx $ \tsk ->
    case status tsk of
      InProgress -> do
        let ref = taskUuid tsk
        updateResult <- updateExistingTask ref $ setTaskStatus Complete

        case updateResult of
          Nothing ->
            pure FailedToModify

          Just t -> do
            V.createEvent (CompleteTask ref) >>= V.writeEvent
            pure $ ModifySuccess t

      _ -> pure FailedToModify


removeTask :: (CanModifyTask m) => Text -> m ModifyResult
removeTask tx =
  withMatch tx $ \tsk ->
    case status tsk of
      Complete -> pure FailedToModify
      Abandoned -> pure FailedToModify
      _ -> do
        let ref = taskUuid tsk
        updateResult <- updateExistingTask ref $ setTaskStatus Abandoned

        case updateResult of
          Nothing ->
            pure FailedToModify

          Just t -> do
            V.createEvent (RemoveTask ref) >>= V.writeEvent
            pure $ ModifySuccess t
