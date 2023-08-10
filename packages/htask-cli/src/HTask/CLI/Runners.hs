{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners (runAction) where

import qualified Data.List                   as List
import qualified Data.Text                   as Text
import qualified Data.UUID                   as UUID
import qualified HTask.Core                  as H

import           Control.Monad.IO.Unlift     (MonadUnliftIO)
import           Control.Monad.Random.Class  (MonadRandom, getRandomR)
import           Data.Foldable               (toList)
import           Data.Function               (on)
import           Data.Tagged                 (untag)
import           Data.Text                   (Text)
import           HTask.CLI.Actions
import           HTask.CLI.App
import           HTask.CLI.Output.Document
import           HTask.CLI.Output.Formatters


runAction :: (MonadRandom m, MonadUnliftIO m) => Action -> App m RunResult
runAction (Add tex)      = runAdd tex
runAction (Complete ref) = runComplete ref
runAction (List d k)     = runList d k
runAction (Remove ref)   = runRemove ref
runAction (Start ref)    = runStart ref
runAction (Stop ref)     = runStop ref
runAction Done           = runDone
runAction Drop           = runDrop
runAction Pick           = runPick
runAction Summary        = runSummary


inProgress :: H.Task -> Bool
inProgress t = H.status t == H.InProgress


taskToText :: H.Task -> Text
taskToText = H.taskUuidToText . H.taskUuid


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


taskPriority :: H.Task -> H.Task -> Ordering
taskPriority = compare `on` H.createdAt


taskDisplayOrder :: H.Task -> H.Task -> Ordering
taskDisplayOrder a b
  = byStatus a b <> byTimestamp a b

  where
    byStatus
      = statusDisplayOrder `on` H.status

    byTimestamp
      = taskPriority


-- TODO: replace with common sense / enum instance
statusDisplayOrder :: H.TaskStatus -> H.TaskStatus -> Ordering
statusDisplayOrder  H.InProgress  H.Abandoned  =  LT
statusDisplayOrder  H.InProgress  H.Complete   =  LT
statusDisplayOrder  H.InProgress  H.Pending    =  LT
statusDisplayOrder  H.InProgress  H.InProgress =  EQ

statusDisplayOrder  H.Pending     H.Abandoned  =  LT
statusDisplayOrder  H.Pending     H.Complete   =  LT
statusDisplayOrder  H.Pending     H.Pending    =  EQ
statusDisplayOrder  H.Pending     H.InProgress =  GT

statusDisplayOrder  H.Complete    H.Abandoned  =  LT
statusDisplayOrder  H.Complete    H.Complete   =  EQ
statusDisplayOrder  H.Complete    H.InProgress =  GT
statusDisplayOrder  H.Complete    H.Pending    =  GT

statusDisplayOrder  H.Abandoned   H.Abandoned  =  EQ
statusDisplayOrder  H.Abandoned   H.Complete   =  GT
statusDisplayOrder  H.Abandoned   H.InProgress =  GT
statusDisplayOrder  H.Abandoned   H.Pending    =  GT


runAdd :: (MonadUnliftIO m) => Text -> App m RunResult
runAdd t
  = formatOutcome <$> H.addTask t

  where
    formatOutcome (H.AddSuccess ref)
      = resultSuccess
          [ "added task: " <> t
          , "ref: " <> H.taskUuidToText ref
          ]

    formatOutcome H.FailedToAdd
      = resultError "failed to add"


runComplete :: (MonadUnliftIO m) => Text -> App m RunResult
runComplete t
  = formatOutcome <$> H.completeTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task ->
            resultSuccess ["completing task: " <> H.description task]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runDone :: (MonadUnliftIO m) => App m RunResult
runDone
  = formatOutcome <$> doneTask

  where
    doneTask
      = H.listTasks >>= mapM (H.completeTask . taskToText) . List.filter inProgress

    formatOutcome
      = resultSuccess . toList . fmap formatRow

    formatRow x
      = case x of
          H.ModifySuccess task ->
            "completing task: " <> H.description task

          H.FailedToFind ->
            "unable to find matching task"

          H.FailedToModify ->
            "unable to modify matching task"


runDrop :: (MonadUnliftIO m) => App m RunResult
runDrop
  = formatOutcome <$> dropTask

  where
    dropTask
      = H.listTasks >>= mapM (H.stopTask . taskToText) . List.filter inProgress

    formatOutcome
      = resultSuccess . toList . fmap formatRow

    formatRow x
      = case x of
          H.ModifySuccess task ->
            "stopping task: " <> H.description task

          H.FailedToFind ->
            "unable to find matching task"

          H.FailedToModify ->
            "unable to modify matching task"


runList :: (Monad m) => ShowUUID -> ShowAll -> App m RunResult
runList showUUID showAll
  = resultSuccess . toList . fmap formatOutput . selectTasks
  <$> H.listTasks

  where
    formatOutput
      = nicePrint showUUID

    selectTasks :: [H.Task] -> [H.Task]
    selectTasks
      = List.sortBy taskDisplayOrder
      . (if untag showAll then id else filterActive)

    filterActive
      = List.filter (justActive . H.status)

    justActive H.Pending    = True
    justActive H.InProgress = True
    justActive H.Complete   = False
    justActive H.Abandoned  = False

    nicePrint :: ShowUUID -> H.Task -> Text
    nicePrint d t
      =  ( if untag d then printUUID else "" )
      <> statusSymbol (H.status t)
      <> " "
      <> withStatusColor (H.status t) (H.description t)

      where
        printUUID = UUID.toText (untag (H.taskUuid t)) <> " "


runPick :: (MonadRandom m, MonadUnliftIO m) => App m RunResult
runPick = do
  tasks <- H.listTasks
  let pendings = List.filter (hasStatus H.Pending) tasks
  k <- randomSelectOne pendings
  maybe
    (pure $ resultError "no task to pick")
    (fmap resultSuccess . startTask)
    k

  where
    startTask t = do
      _ <- H.startTask $ taskToText t
      pure ["picking task: " <> H.description t]

    randomSelectOne :: (MonadRandom m) => [a] -> m (Maybe a)
    randomSelectOne [] = pure Nothing
    randomSelectOne xs =
      (`maybeAt` xs) <$> getRandomR (0, List.length xs)

    maybeAt :: Int -> [a] -> Maybe a
    maybeAt _ [] = Nothing
    maybeAt n xs
      | n < 0 = Nothing
      | n >= List.length xs = Nothing
      | otherwise = Just (xs !! n)


runRemove :: (MonadUnliftIO m) => Text -> App m RunResult
runRemove t
  = formatOutcome <$> H.removeTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task ->
            resultSuccess ["removing task: " <> H.description task]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runStart :: (MonadUnliftIO m) => Text -> App m RunResult
runStart t
  = formatOutcome <$> H.startTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task ->
            resultSuccess ["starting task: " <> H.description task]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runStop :: (MonadUnliftIO m) => Text -> App m RunResult
runStop t
  = formatOutcome <$> H.stopTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task ->
            resultSuccess ["stopping task: " <> H.description task]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"




runSummary :: (Monad m) => App m RunResult
runSummary
  = renderSummary <$> H.listTasks

  where
    renderSummary :: [H.Task] -> RunResult
    renderSummary tasks = resultSuccess
      $ displayCurrent
      <> displayTopPending

      where
        displayCurrent :: [Text]
        displayCurrent = do
          let actives = List.filter (hasStatus H.InProgress) tasks
          if List.null actives
            then
              [ "No current task" ]
            else
              "Current task:"
              : concatMap printTaskForSummary actives


        displayTopPending :: [Text]
        displayTopPending
          = pendingMessage (length xs) (length pendings)
          : concatMap printTaskForSummary xs

          where
            pendings = List.sortBy taskPriority (List.filter (hasStatus H.Pending) tasks)

            xs = List.take 5 pendings

            pendingMessage x p =
              "Top " <> tInt x <> " pending (" <> tInt (p - x) <> " hidden):"

            tInt = Text.pack . show


        printTaskForSummary :: H.Task -> [Text]
        printTaskForSummary t =
          [ indent printDescription
          , indent $ indent printRef
          ]

          where
            printDescription :: Text
            printDescription
              =  statusSymbol (H.status t)
              <> " "
              <> withStatusColor (H.status t) (H.description t)

            printRef :: Text
            printRef = (UUID.toText . untag . H.taskUuid) t
