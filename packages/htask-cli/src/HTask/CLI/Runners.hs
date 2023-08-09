{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTask.CLI.Runners (runAction) where

import qualified Data.Sequence               as Seq
import qualified Data.Text                   as Text
import qualified Data.UUID                   as UUID
import qualified HTask.Core                  as H
import qualified Leadpixel.Events            as V

import           Control.Monad.IO.Unlift     (MonadUnliftIO)
import           Control.Monad.Random.Class  (MonadRandom, getRandomR)
import           Data.Foldable               (toList)
import           Data.Function               (on)
import           Data.List                   (null)
import           Data.Sequence               (Seq, (!?))
import           Data.Tagged                 (untag)
import           Data.Text                   (Text)
import           Data.Time                   (UTCTime)
import           Data.UUID                   (UUID)
import           HTask.CLI.Actions
import           HTask.CLI.Output.Document
import           HTask.CLI.Output.Formatters
import           HTask.CLI.TaskApplication
import           Leadpixel.Provider


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


runAdd ::
  ( Functor m
  , Provider UTCTime m
  , Provider UUID m
  , V.HasEventSink m
  , H.HasTasks m
  ) => Text -> m RunResult
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


runComplete :: (H.CanCreateTask m, V.HasEventSource m, V.HasEventSink m, H.HasTasks m) => Text -> m RunResult
runComplete t
  = formatOutcome <$> H.completeTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["completing task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runDone :: (H.CanCreateTask m, V.HasEventSource m, V.HasEventSink m, H.HasTasks m) => m RunResult
runDone
  = formatOutcome <$> doneTask

  where
    doneTask
      = H.listTasks >>= mapM (H.completeTask . taskToText) . Seq.filter inProgress

    formatOutcome
      = resultSuccess . toList . fmap formatRow

    formatRow x
      = case x of
          H.ModifySuccess tsk ->
            "completing task: " <> H.description tsk

          H.FailedToFind ->
            "unable to find matching task"

          H.FailedToModify ->
            "unable to modify matching task"


runDrop :: (H.CanCreateTask m, V.HasEventSource m, V.HasEventSink m, H.HasTasks m) => m RunResult
runDrop
  = formatOutcome <$> dropTask

  where
    dropTask
      = H.listTasks >>= mapM (H.stopTask . taskToText) . Seq.filter inProgress

    formatOutcome
      = resultSuccess . toList . fmap formatRow

    formatRow x
      = case x of
          H.ModifySuccess tsk ->
            "stopping task: " <> H.description tsk

          H.FailedToFind ->
            "unable to find matching task"

          H.FailedToModify ->
            "unable to modify matching task"




runList :: (Functor m, H.HasTasks m) => ShowUUID -> ShowAll -> m RunResult
runList showUUID showAll
  = resultSuccess . toList . fmap formatOutput . selectTasks
  <$> H.listTasks

  where
    formatOutput
      = nicePrint showUUID

    selectTasks :: Seq H.Task -> Seq H.Task
    selectTasks
      = Seq.sortBy taskDisplayOrder
      . (if untag showAll then id else filterActive)

    filterActive
      = Seq.filter (justActive . H.status)

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


runPick :: (MonadRandom m, H.CanCreateTask m, V.HasEventSource m, V.HasEventSink m, H.HasTasks m) => m RunResult
runPick = do
  ts <- H.listTasks
  let ps = Seq.filter (hasStatus H.Pending) ts
  k <- randomSelectOne ps
  maybe
    (pure $ resultError "no task to pick")
    (fmap resultSuccess . startTask)
    k

  where
    startTask t = do
      _ <- H.startTask $ taskToText t
      pure ["picking task: " <> H.description t]


randomSelectOne :: (Monad m, MonadRandom m) => Seq a -> m (Maybe a)
randomSelectOne Seq.Empty = pure Nothing
randomSelectOne xs =
  (xs !?) <$> getRandomR (0, Seq.length xs)


runRemove :: (H.CanCreateTask m, V.HasEventSource m, V.HasEventSink m, H.HasTasks m) => Text -> m RunResult
runRemove t
  = formatOutcome <$> H.removeTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["removing task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runStart :: (H.CanCreateTask m, V.HasEventSource m, V.HasEventSink m, H.HasTasks m) => Text -> m RunResult
runStart t
  = formatOutcome <$> H.startTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["starting task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runStop :: (V.HasEventSource m, V.HasEventSink m, Provider UTCTime m, H.HasTasks m) => Text -> m RunResult
runStop t
  = formatOutcome <$> H.stopTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["stopping task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"




runSummary :: (Functor m, H.HasTasks m) => m RunResult
runSummary
  = renderSummary <$> H.listTasks

  where
    renderSummary :: Seq H.Task -> RunResult
    renderSummary ts = resultSuccess
      $ displayCurrent
      <> displayTopPending

      where
        displayCurrent :: [Text]
        displayCurrent = do
          let ps = Seq.filter (hasStatus H.InProgress) ts
          if Data.List.null ps
            then
              [ "No current task" ]
            else
              "Current task:"
              : concatMap printTaskForSummary ps


        displayTopPending :: [Text]
        displayTopPending
          = pendingMessage (length xs) (length ps)
          : concatMap printTaskForSummary xs

          where
            ps = Seq.sortBy taskPriority (Seq.filter (hasStatus H.Pending) ts)

            xs = Seq.take 5 ps

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
