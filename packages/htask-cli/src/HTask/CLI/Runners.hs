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

import           Control.Monad.IO.Class      (MonadIO)
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


runAction
  :: (HasEventBackend m, H.CanCreateTask m, MonadRandom m)
  => Action -> m RunResult
runAction Summary        = runSummary
runAction (List d k)     = runList d k

runAction (Add tex)      = runAdd tex
runAction (Start ref)    = runStart ref
runAction (Stop ref)     = runStop ref
runAction (Complete ref) = runComplete ref
runAction (Remove ref)   = runRemove ref

runAction Pick           = runPick
runAction Drop           = runDrop
runAction Done           = runDone


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


runAdd :: (Functor m, MonadIO m, V.HasEventSource m, Provider UTCTime m, Provider UUID m, V.HasEventSink m) => Text -> m RunResult
runAdd t
  = formatOutcome <$> runTask (H.addTask t)

  where
    formatOutcome (H.AddSuccess ref)
      = resultSuccessAdd ref

    formatOutcome H.FailedToAdd
      = resultError "failed to add"

    resultSuccessAdd ref
      = resultSuccess
          [ "added task: " <> t
          , "ref: " <> H.taskUuidToText ref
          ]


runComplete :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runComplete t
  = formatOutcome <$> runTask (H.completeTask t)

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["completing task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runDone :: (HasEventBackend m, H.CanCreateTask m) => m RunResult
runDone
  = formatOutcome <$> runTask doneTask

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


runDrop :: (HasEventBackend m, H.CanCreateTask m) => m RunResult
runDrop
  = formatOutcome <$> runTask dropTask

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




runList :: (MonadIO m, V.HasEventSource m) => ShowUUID -> ShowAll -> m RunResult
runList showUUID showAll
  = resultSuccess . toList . fmap formatOutput . selectTasks
  <$> runTask H.listTasks

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


runPick :: (MonadRandom m, HasEventBackend m, H.CanCreateTask m) => m RunResult
runPick = do
  ts <- runTask H.listTasks
  let ps = Seq.filter (hasStatus H.Pending) ts
  k <- randomSelectOne ps
  maybe
    (pure $ resultError "no task to pick")
    (fmap resultSuccess . startTask)
    k

  where
    startTask t = do
      _ <- runTask (H.startTask $ taskToText t)
      pure ["picking task: " <> H.description t]


randomSelectOne :: (Monad m, MonadRandom m) => Seq a -> m (Maybe a)
randomSelectOne Seq.Empty = pure Nothing
randomSelectOne xs =
  (xs !?) <$> getRandomR (0, Seq.length xs)


runRemove :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runRemove t
  = formatOutcome <$> runTask (H.removeTask t)

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["removing task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runStart :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runStart t
  = formatOutcome <$> runTask (H.startTask t)

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["starting task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runStop :: (MonadIO m, HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runStop t
  = formatOutcome <$> runTask (H.stopTask t)

  where
    formatOutcome x
      = case x of
          H.ModifySuccess tsk ->
            resultSuccess ["stopping task: " <> H.description tsk]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"




runSummary :: (MonadIO m, HasEventBackend m) => m RunResult
runSummary
  = renderSummary <$> runTask H.listTasks


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
