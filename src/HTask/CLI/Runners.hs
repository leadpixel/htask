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
import           HTask.CLI.Output


runAction :: (CanRunAction m) => Action -> m RunResult
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


tInt :: Int -> Text
tInt = Text.pack . show


-- | Helper to format a task entry with metadata on one line and description indented under it
formatTaskEntry :: Bool -> Map.Map H.TaskUuid Text -> [H.Task] -> H.Task -> [Text]
formatTaskEntry showFullUuid prefs allTs t =
  [ headerLine
  ] <> descriptionLines

  where
    idx = maybe "?" (tInt . (+1)) (List.findIndex (\x -> H.taskUuid x == H.taskUuid t) allTs)
    symbol = statusSymbol (H.status t)

    -- We always show short UUID, showFullUuid toggles the full text
    uuidText = if showFullUuid
               then H.taskUuidToText (H.taskUuid t)
               else Map.findWithDefault (H.taskUuidToText (H.taskUuid t)) (H.taskUuid t) prefs

    -- Consistent coloring for metadata
    headerLine = withStatusColor (H.status t) (padLeft 3 idx <> " " <> symbol) <> " " <> withDim uuidText

    descriptionLines = fmap (\l -> "      " <> withStatusColor (H.status t) l) (Text.lines (H.description t))


runAdd :: (CanRunAction m) => Text -> m RunResult
runAdd t
  = formatOutcome <$> H.addTask t

  where
    formatOutcome (H.AddSuccess ref)
      = resultSuccess
          [ withStatusColor H.Pending "added task: " <> withBold t
          , withDim ("      ref: " <> H.taskUuidToText ref)
          ]

    formatOutcome H.FailedToAdd
      = resultError "failed to add"

    formatOutcome H.EmptyDescription
      = resultError "task description cannot be empty"


runComplete :: (CanRunAction m) => Text -> m RunResult
runComplete t
  = formatOutcome <$> H.completeTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task ->
            resultSuccess [withStatusColor H.Complete "completing task: " <> withBold (H.description task)]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runDone :: (CanRunAction m) => m RunResult
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
            withStatusColor H.Complete "completing task: " <> withBold (H.description task)

          H.FailedToFind ->
            "unable to find matching task"

          H.FailedToModify ->
            "unable to modify matching task"


runDrop :: (CanRunAction m) => m RunResult
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
            withStatusColor H.Pending "stopping task: " <> withBold (H.description task)

          H.FailedToFind ->
            "unable to find matching task"

          H.FailedToModify ->
            "unable to modify matching task"


runList :: (CanRunAction m) => ShowUUID -> ShowAll -> m RunResult
runList showUUID showAll = do
  allTasks <- List.sortBy H.taskDisplayOrder <$> H.listTasks
  let filteredTasks = selectTasks allTasks
  let prefixes = H.disambiguatingPrefixes (H.taskUuid <$> allTasks)
  pure $ formatList allTasks filteredTasks prefixes

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
        gs -> ("\n" <> withBold (statusHeader s)) : divider : concatMap (nicePrint prefs allTs) gs

    statusHeader H.InProgress = "In Progress"
    statusHeader H.Pending    = "Pending"
    statusHeader H.Complete   = "Completed"
    statusHeader H.Abandoned  = "Abandoned"

    nicePrint :: Map.Map H.TaskUuid Text -> [H.Task] -> H.Task -> [Text]
    nicePrint = formatTaskEntry (untag showUUID)


runPick :: (CanRunAction m) => m RunResult
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
      pure [withStatusColor H.InProgress "picking task: " <> withBold (H.description t)]

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


runRemove :: (CanRunAction m) => Text -> m RunResult
runRemove t
  = formatOutcome <$> H.removeTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task ->
            resultSuccess [withStatusColor H.Abandoned "removing task: " <> withBold (H.description task)]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runStart :: (CanRunAction m) => Text -> m RunResult
runStart t
  = formatOutcome <$> H.startTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task ->
            resultSuccess [withStatusColor H.InProgress "starting task: " <> withBold (H.description task)]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"


runStop :: (CanRunAction m) => Text -> m RunResult
runStop t
  = formatOutcome <$> H.stopTask t

  where
    formatOutcome x
      = case x of
          H.ModifySuccess task ->
            resultSuccess [withStatusColor H.Pending "stopping task: " <> withBold (H.description task)]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"




runSummary :: (CanRunAction m) => m RunResult
runSummary = do
  tasks <- H.listTasks
  let allSorted = List.sortBy H.taskDisplayOrder tasks
  let actives = List.filter (hasStatus H.InProgress) allSorted
  let pendings = List.filter (hasStatus H.Pending) allSorted
  let topPendings = List.take 5 pendings

  let prefixes = H.disambiguatingPrefixes (H.taskUuid <$> allSorted)

  pure $ renderSummary allSorted actives topPendings prefixes

  where
    renderSummary allTasks actives topPendings prefixes = resultSuccess
      $ displayCurrent
      <> displayTopPending

      where
        displayCurrent :: [Text]
        displayCurrent =
          if List.null actives
            then [ "No current task" ]
            else withBold "Current task" : divider : concatMap (formatTaskEntry False prefixes allTasks) actives


        displayTopPending :: [Text]
        displayTopPending =
          let totalPendings = List.length (List.filter (hasStatus H.Pending) allTasks)
          in ("\n" <> withBold (pendingMessage (length topPendings) totalPendings))
             : divider
             : concatMap (formatTaskEntry False prefixes allTasks) topPendings

          where
            pendingMessage x p =
              "Pending tasks (Top " <> tInt x <> " of " <> tInt p <> ")"
