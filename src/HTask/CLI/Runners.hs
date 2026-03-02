{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.Runners (runAction) where

import qualified Data.List                  as List
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as Text
import qualified HTask.Core                 as H

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


inProgress :: H.Task -> Bool
inProgress t = H.status t == H.InProgress


taskToText :: H.Task -> Text
taskToText = H.taskUuidToText . H.taskUuid


hasStatus :: H.TaskStatus -> H.Task -> Bool
hasStatus s t = s == H.status t


tInt :: Int -> Text
tInt = Text.pack . show


-- | Helper to format a task entry with metadata on one line and description indented under it
formatTaskEntry :: Bool -> Map.Map H.TaskUuid Text -> [H.Task] -> H.Task -> [Text]
formatTaskEntry showFullUuid prefs allTs t =
  [ headerLine
  ] <> descriptionLines

  where
    idx = maybe "??" (padZero 2 . tInt . (+1)) (List.findIndex (\x -> H.taskUuid x == H.taskUuid t) allTs)
    symbol = statusSymbol (H.status t)

    -- We always show short UUID, showFullUuid toggles the full text
    uuidText = if showFullUuid
               then H.taskUuidToText (H.taskUuid t)
               else Map.findWithDefault (H.taskUuidToText (H.taskUuid t)) (H.taskUuid t) prefs

    -- Header: " 01 ▶ 9bb2"
    headerLine = "  " <> withStatusColor (H.status t) (idx <> " " <> symbol) <> " " <> withDim uuidText

    -- Description: "      ╰─ bash autocompletion"
    descriptionLines = case Text.lines (H.description t) of
      [] -> []
      (f:fs) -> ("       " <> treeLink <> withBold f)
              : fmap (\l -> "          " <> withBold l) fs


runAdd :: (CanRunAction m) => Bool -> Text -> m RunResult
runAdd j t
  = formatOutcome <$> H.addTask t

  where
    formatOutcome (H.AddSuccess ref)
      | j = resultJson ref
      | otherwise = resultSuccess
          [ withStatusColor H.Pending "added task: " <> withBold t
          , withDim ("      ref: " <> H.taskUuidToText ref)
          ]

    formatOutcome H.FailedToAdd
      = resultError "failed to add"

    formatOutcome H.EmptyDescription
      = resultError "task description cannot be empty"


runComplete :: (CanRunAction m) => Bool -> Text -> m RunResult
runComplete j t
  = formatOutcome <$> H.completeTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task
            | j -> resultJson task
            | otherwise -> resultSuccess [withStatusColor H.Complete "completing task: " <> withBold (H.description task)]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runDone :: (CanRunAction m) => Bool -> m RunResult
runDone j
  = formatOutcome <$> doneTask

  where
    doneTask
      = H.listTasks >>= mapM (H.completeTask . taskToText) . List.filter inProgress

    formatOutcome xs
      | j = resultJson (toList (fmap getTask xs))
      | otherwise = resultSuccess . toList . fmap formatRow $ xs

    getTask (H.ModifySuccess t) = Just t
    getTask _                   = Nothing

    formatRow x
      = case x of
          H.ModifySuccess task ->
            withStatusColor H.Complete "completing task: " <> withBold (H.description task)

          H.FailedToFind ->
            "unable to find matching task"

          H.FailedToModify ->
            "unable to modify matching task"


runDrop :: (CanRunAction m) => Bool -> m RunResult
runDrop j
  = formatOutcome <$> dropTask

  where
    dropTask
      = H.listTasks >>= mapM (H.stopTask . taskToText) . List.filter inProgress

    formatOutcome xs
      | j = resultJson (toList (fmap getTask xs))
      | otherwise = resultSuccess . toList . fmap formatRow $ xs

    getTask (H.ModifySuccess t) = Just t
    getTask _                   = Nothing

    formatRow x
      = case x of
          H.ModifySuccess task ->
            withStatusColor H.Pending "stopping task: " <> withBold (H.description task)

          H.FailedToFind ->
            "unable to find matching task"

          H.FailedToModify ->
            "unable to modify matching task"


runList :: (CanRunAction m) => Bool -> ShowUUID -> ShowAll -> m RunResult
runList j showUUID showAll = do
  allTasks <- List.sortBy H.taskDisplayOrder <$> H.listTasks
  let filteredTasks = selectTasks allTasks
  let prefixes = H.disambiguatingPrefixes (H.taskUuid <$> allTasks)

  if j
    then pure $ resultJson filteredTasks
    else pure $ formatList allTasks filteredTasks prefixes

  where
    selectTasks
      = if untag showAll then id else List.filter (justActive . H.status)

    justActive H.Pending    = True
    justActive H.InProgress = True
    justActive _            = False

    formatList :: [H.Task] -> [H.Task] -> Map.Map H.TaskUuid Text -> RunResult
    formatList allTs ts prefs = resultSuccess $
      concatMap (formatGroup allTs ts prefs) [H.InProgress, H.Pending, H.Complete, H.Abandoned]

    formatGroup :: [H.Task] -> [H.Task] -> Map.Map H.TaskUuid Text -> H.TaskStatus -> [Text]
    formatGroup allTs ts prefs s =
      case List.filter (hasStatus s) ts of
        [] -> []
        gs -> ("\n" <> divider (statusHeader s)) : concatMap (nicePrint prefs allTs) gs

    statusHeader H.InProgress = "IN PROGRESS"
    statusHeader H.Pending    = "PENDING"
    statusHeader H.Complete   = "COMPLETED"
    statusHeader H.Abandoned  = "ABANDONED"

    nicePrint :: Map.Map H.TaskUuid Text -> [H.Task] -> H.Task -> [Text]
    nicePrint = formatTaskEntry (untag showUUID)


runPick :: (CanRunAction m) => Bool -> m RunResult
runPick j = do
  tasks <- H.listTasks
  let pendings = List.filter (hasStatus H.Pending) tasks
  k <- randomSelectOne pendings
  maybe
    (pure $ resultError "no task to pick")
    (fmap formatOutcome . startTask)
    k

  where
    formatOutcome t
      | j = resultJson t
      | otherwise = resultSuccess [withStatusColor H.InProgress "picking task: " <> withBold (H.description t)]

    startTask t = do
      _ <- H.startTask $ taskToText t
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
  = formatOutcome <$> H.removeTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task
            | j -> resultJson task
            | otherwise -> resultSuccess [withStatusColor H.Abandoned "removing task: " <> withBold (H.description task)]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runStart :: (CanRunAction m) => Bool -> Text -> m RunResult
runStart j t
  = formatOutcome <$> H.startTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task
            | j -> resultJson task
            | otherwise -> resultSuccess [withStatusColor H.InProgress "starting task: " <> withBold (H.description task)]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runStop :: (CanRunAction m) => Bool -> Text -> m RunResult
runStop j t
  = formatOutcome <$> H.stopTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task
            | j -> resultJson task
            | otherwise -> resultSuccess [withStatusColor H.Pending "stopping task: " <> withBold (H.description task)]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"




runSummary :: (CanRunAction m) => Bool -> m RunResult
runSummary j = do
  tasks <- H.listTasks
  let allSorted = List.sortBy H.taskDisplayOrder tasks
  let actives = List.filter (hasStatus H.InProgress) allSorted
  let pendings = List.filter (hasStatus H.Pending) allSorted
  let topPendings = List.take 5 pendings

  let prefixes = H.disambiguatingPrefixes (H.taskUuid <$> allSorted)

  if j
    then pure $ resultJson (actives <> topPendings)
    else pure $ renderSummary allSorted actives topPendings prefixes

  where
    renderSummary allTasks actives topPendings prefixes = resultSuccess
      $ displayCurrent
      <> displayTopPending

      where
        displayCurrent :: [Text]
        displayCurrent =
          if List.null actives
            then [ "No current task" ]
            else divider "CURRENT TASK" : concatMap (formatTaskEntry False prefixes allTasks) actives


        displayTopPending :: [Text]
        displayTopPending =
          let totalPendings = List.length (List.filter (hasStatus H.Pending) allTasks)
          in ("\n" <> divider (pendingMessage (length topPendings) totalPendings))
             : concatMap (formatTaskEntry False prefixes allTasks) topPendings

          where
            pendingMessage x p =
              "PENDING TASKS (" <> tInt x <> " OF " <> tInt p <> ")"
