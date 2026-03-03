{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners (runAction) where

import qualified Data.List                  as List
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as Text
import qualified HTask.Core                 as Core

import           Control.Monad.Random.Class (MonadRandom, getRandomR)
import           Data.Foldable              (toList)
import           Data.Tagged                (untag)
import           Data.Text                  (Text)
import           HTask.CLI.Actions
import           HTask.CLI.App
import           HTask.CLI.Options          (Options (..))
import           HTask.CLI.Output

runAction :: (CanRunAction m) => Options -> m RunResult
runAction opts =
  let j = useJson opts
  in case action opts of
    (Add tex)      -> runAdd j tex
    (Complete ref) -> runComplete j ref
    (List d k)     -> runList j d k
    (Remove ref)   -> runRemove j ref
    (Start ref)    -> runStart j ref
    (Stop ref)     -> runStop j ref
    Done           -> runDone j
    Drop           -> runDrop j
    Pick           -> runPick j
    Summary        -> runSummary j

inProgress :: Core.Task -> Bool
inProgress t = Core.status t == Core.InProgress

taskToText :: Core.Task -> Text
taskToText = Core.taskUuidToText . Core.taskUuid

hasStatus :: Core.TaskStatus -> Core.Task -> Bool
hasStatus s t = s == Core.status t

tInt :: Int -> Text
tInt = Text.pack . show

-- | Helper to format a task entry with metadata on one line and description indented under it
formatTaskEntry :: Bool -> Map.Map Core.TaskUuid Text -> [Core.Task] -> Core.Task -> [Text]
formatTaskEntry _ _ idRef t =
  [ headerLine
  ] <> descriptionLines

  where
    idx = maybe "??" (padZero 2 . tInt . (+1)) (List.findIndex (\x -> Core.taskUuid x == Core.taskUuid t) idRef)
    symbol = statusSymbol (Core.status t)

    uuidText = Core.taskUuidToText (Core.taskUuid t)

    -- Header: " 01 ▶ 9bb2"
    headerLine = "  " <> withStatusColor (Core.status t) (idx <> " " <> symbol) <> " " <> withDim uuidText

    -- Description: "      ╰─ bash autocompletion"
    descriptionLines = case Text.lines (Core.description t) of
      [] -> []
      (f:fs) -> ("       " <> treeLink <> withBold f)
              : fmap (\l -> "          " <> withBold l) fs

runAdd :: (CanRunAction m) => Bool -> Text -> m RunResult
runAdd j t
  = formatOutcome <$> Core.addTask t

  where
    formatOutcome (Core.AddSuccess ref)
      | j = resultJson ref
      | otherwise = resultSuccess
          [ withStatusColor Core.Pending "added task: " <> withBold t
          , withDim ("      ref: " <> Core.taskUuidToText ref)
          ]

    formatOutcome Core.FailedToAdd
      = resultError "failed to add"

    formatOutcome Core.EmptyDescription
      = resultError "task description cannot be empty"

runComplete :: (CanRunAction m) => Bool -> Text -> m RunResult
runComplete j t
  = formatOutcome <$> Core.completeTask t

  where
    formatOutcome x
      = case x of
          Core.ModifySuccess task
            | j -> resultJson task
            | otherwise -> resultSuccess [withStatusColor Core.Complete "completing task: " <> withBold (Core.description task)]

          Core.FailedToFind ->
            resultError "unable to find matching task"

          Core.FailedToModify ->
            resultError "unable to modify matching task"

runDone :: (CanRunAction m) => Bool -> m RunResult
runDone j
  = formatOutcome <$> doneTask

  where
    doneTask
      = Core.listTasks >>= mapM (Core.completeTask . taskToText) . List.filter inProgress

    formatOutcome xs
      | j = resultJson (toList (fmap getTask xs))
      | otherwise = resultSuccess . toList . fmap formatRow $ xs

    getTask (Core.ModifySuccess t) = Just t
    getTask _                      = Nothing

    formatRow x
      = case x of
          Core.ModifySuccess task ->
            withStatusColor Core.Complete "completing task: " <> withBold (Core.description task)

          Core.FailedToFind ->
            "unable to find matching task"

          Core.FailedToModify ->
            "unable to modify matching task"

runDrop :: (CanRunAction m) => Bool -> m RunResult
runDrop j
  = formatOutcome <$> dropTask

  where
    dropTask
      = Core.listTasks >>= mapM (Core.stopTask . taskToText) . List.filter inProgress

    formatOutcome xs
      | j = resultJson (toList (fmap getTask xs))
      | otherwise = resultSuccess . toList . fmap formatRow $ xs

    getTask (Core.ModifySuccess t) = Just t
    getTask _                      = Nothing

    formatRow x
      = case x of
          Core.ModifySuccess task ->
            withStatusColor Core.Pending "stopping task: " <> withBold (Core.description task)

          Core.FailedToFind ->
            "unable to find matching task"

          Core.FailedToModify ->
            "unable to modify matching task"

runList :: (CanRunAction m) => Bool -> ShowUUID -> ShowAll -> m RunResult
runList j showUUID showAll = do
  allTasks <- Core.listTasks
  -- The 'reference' list for IDs is sorted by creation time so indices NEVER change
  let idReference = List.sortBy Core.taskPriority allTasks
  let displayTasks = List.sortBy Core.taskDisplayOrder (selectTasks allTasks)
  let prefixes = Core.disambiguatingPrefixes (Core.taskUuid <$> allTasks)

  if j
    then pure $ resultJson displayTasks
    else pure $ formatList idReference displayTasks prefixes

  where
    selectTasks
      = if untag showAll then id else List.filter (justActive . Core.status)

    justActive Core.Pending    = True
    justActive Core.InProgress = True
    justActive _               = False

    formatList :: [Core.Task] -> [Core.Task] -> Map.Map Core.TaskUuid Text -> RunResult
    formatList idRef ts prefs = resultSuccess $
      concatMap (formatGroup idRef ts prefs) [Core.InProgress, Core.Pending, Core.Complete, Core.Abandoned]

    formatGroup :: [Core.Task] -> [Core.Task] -> Map.Map Core.TaskUuid Text -> Core.TaskStatus -> [Text]
    formatGroup idRef ts prefs s =
      case List.filter (hasStatus s) ts of
        [] -> []
        gs -> ("\n" <> divider (statusHeader s)) : concatMap (nicePrint prefs idRef) gs

    statusHeader Core.InProgress = "IN PROGRESS"
    statusHeader Core.Pending    = "PENDING"
    statusHeader Core.Complete   = "COMPLETED"
    statusHeader Core.Abandoned  = "ABANDONED"

    nicePrint :: Map.Map Core.TaskUuid Text -> [Core.Task] -> Core.Task -> [Text]
    nicePrint = formatTaskEntry (untag showUUID)

runPick :: (CanRunAction m) => Bool -> m RunResult
runPick j = do
  tasks <- Core.listTasks
  let pendings = List.filter (hasStatus Core.Pending) tasks
  k <- randomSelectOne pendings
  maybe
    (pure $ resultError "no task to pick")
    (fmap formatOutcome . startTask)
    k

  where
    formatOutcome t
      | j = resultJson t
      | otherwise = resultSuccess [withStatusColor Core.InProgress "picking task: " <> withBold (Core.description t)]

    startTask t = do
      _ <- Core.startTask $ taskToText t
      pure t

    randomSelectOne :: (MonadRandom m) => [a] -> m (Maybe a)
    randomSelectOne [] = pure Nothing
    randomSelectOne xs =
      (\i -> xs `maybeAt` i) <$> getRandomR (0, List.length xs - 1)

    maybeAt :: [a] -> Int -> Maybe a
    maybeAt [] _ = Nothing
    maybeAt xs n
      | n < 0 = Nothing
      | n >= List.length xs = Nothing
      | otherwise = Just (xs !! n)

runRemove :: (CanRunAction m) => Bool -> Text -> m RunResult
runRemove j t
  = formatOutcome <$> Core.removeTask t

  where
    formatOutcome x
      = case x of
          Core.ModifySuccess task
            | j -> resultJson task
            | otherwise -> resultSuccess [withStatusColor Core.Abandoned "removing task: " <> withBold (Core.description task)]

          Core.FailedToFind ->
            resultError "unable to find matching task"

          Core.FailedToModify ->
            resultError "unable to modify matching task"

runStart :: (CanRunAction m) => Bool -> Text -> m RunResult
runStart j t
  = formatOutcome <$> Core.startTask t

  where
    formatOutcome x
      = case x of
          Core.ModifySuccess task
            | j -> resultJson task
            | otherwise -> resultSuccess [withStatusColor Core.InProgress "starting task: " <> withBold (Core.description task)]

          Core.FailedToFind ->
            resultError "unable to find matching task"

          Core.FailedToModify ->
            resultError "unable to modify matching task"

runStop :: (CanRunAction m) => Bool -> Text -> m RunResult
runStop j t
  = formatOutcome <$> Core.stopTask t

  where
    formatOutcome x
      = case x of
          Core.ModifySuccess task
            | j -> resultJson task
            | otherwise -> resultSuccess [withStatusColor Core.Pending "stopping task: " <> withBold (Core.description task)]

          Core.FailedToFind ->
            resultError "unable to find matching task"

          Core.FailedToModify ->
            resultError "unable to modify matching task"

runSummary :: (CanRunAction m) => Bool -> m RunResult
runSummary j = do
  tasks <- Core.listTasks
  let idReference = List.sortBy Core.taskPriority tasks
  let allSorted = List.sortBy Core.taskDisplayOrder tasks
  let actives = List.filter (hasStatus Core.InProgress) allSorted
  let pendings = List.filter (hasStatus Core.Pending) allSorted
  let topPendings = List.take 5 pendings

  let prefixes = Core.disambiguatingPrefixes (Core.taskUuid <$> allSorted)

  if j
    then pure $ resultJson (actives <> topPendings)
    else pure $ renderSummary idReference allSorted actives topPendings prefixes

  where
    renderSummary idRef allTasks actives topPendings prefixes = resultSuccess
      $ displayCurrent
      <> displayTopPending

      where
        displayCurrent :: [Text]
        displayCurrent =
          if List.null actives
            then [ "No current task" ]
            else divider "CURRENT TASK" : concatMap (formatTaskEntry False prefixes idRef) actives


        displayTopPending :: [Text]
        displayTopPending =
          let totalPendings = List.length (List.filter (hasStatus Core.Pending) allTasks)
          in ("\n" <> divider (pendingMessage (length topPendings) totalPendings))
             : concatMap (formatTaskEntry False prefixes idRef) topPendings

          where
            pendingMessage x p =
              "PENDING TASKS (" <> tInt x <> " OF " <> tInt p <> ")"
