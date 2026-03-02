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


-- | Helper to format descriptions with consistent indentation for newlines
formatDescription :: Int -> H.Task -> Text
formatDescription padding t =
  let lines' = Text.lines (H.description t)
      symbol = statusSymbol (H.status t)
      colorized = withStatusColor (H.status t)
      firstLine = symbol <> " " <> colorized (head lines')
      otherLines = fmap (\l -> Text.replicate padding " " <> colorized l) (tail lines')
  in Text.intercalate "\n" (firstLine : otherLines)


runAdd :: (CanRunAction m) => Text -> m RunResult
runAdd t
  = formatOutcome <$> H.addTask t

  where
    formatOutcome (H.AddSuccess ref)
      = resultSuccess
          [ "added task: " <> withBold t
          , "ref: " <> withDim (H.taskUuidToText ref)
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
            resultSuccess ["completing task: " <> withBold (H.description task)]

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
            "completing task: " <> withBold (H.description task)

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
            "stopping task: " <> withBold (H.description task)

          H.FailedToFind ->
            "unable to find matching task"

          H.FailedToModify ->
            "unable to modify matching task"


runList :: (CanRunAction m) => ShowUUID -> ShowAll -> m RunResult
runList showUUID showAll = do
  allTasks <- List.sortBy H.taskDisplayOrder <$> H.listTasks
  let tasks = selectTasks allTasks
  let prefixes = H.disambiguatingPrefixes (H.taskUuid <$> allTasks)
  pure $ formatList allTasks tasks prefixes

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
        gs -> ("\n" <> withBold (statusHeader s)) : fmap (nicePrint prefs allTs) gs

    statusHeader H.InProgress = "In Progress"
    statusHeader H.Pending    = "Pending"
    statusHeader H.Complete   = "Completed"
    statusHeader H.Abandoned  = "Abandoned"

    nicePrint :: Map.Map H.TaskUuid Text -> [H.Task] -> H.Task -> Text
    nicePrint prefs allTs t
      =  padLeft 3 (maybe "?" tInt (taskIndex t allTs)) <> " "
      <> formatDescription 6 t
      <> (if untag showUUID then "\n    " <> withDim (Map.findWithDefault "" (H.taskUuid t) prefs) else "")

    taskIndex t allTs = (+1) <$> List.findIndex (\x -> H.taskUuid x == H.taskUuid t) allTs


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
      pure ["picking task: " <> withBold (H.description t)]

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
            resultSuccess ["removing task: " <> withBold (H.description task)]

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
            resultSuccess ["starting task: " <> withBold (H.description task)]

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
            resultSuccess ["stopping task: " <> withBold (H.description task)]

          H.FailedToFind ->
            resultError "unable to find matching task"

          H.FailedToModify ->
            resultError "unable to modify matching task"




runSummary :: (CanRunAction m) => m RunResult
runSummary = do
  tasks <- H.listTasks
  let actives = List.filter (hasStatus H.InProgress) tasks
  let pendings = List.sortBy H.taskPriority (List.filter (hasStatus H.Pending) tasks)
  let topPendings = List.take 5 pendings

  let prefixes = H.disambiguatingPrefixes (H.taskUuid <$> tasks)

  pure $ renderSummary tasks actives topPendings prefixes

  where
    renderSummary allTasks actives topPendings prefixes = resultSuccess
      $ displayCurrent
      <> displayTopPending

      where
        displayCurrent :: [Text]
        displayCurrent =
          if List.null actives
            then [ "No current task" ]
            else withBold "Current task:" : concatMap (printTaskForSummary allTasks prefixes) actives


        displayTopPending :: [Text]
        displayTopPending
          = ("\n" <> withBold (pendingMessage (length topPendings) (length pendings)))
          : concatMap (printTaskForSummary allTasks prefixes) topPendings

          where
            pendings = List.filter (hasStatus H.Pending) allTasks
            pendingMessage x p =
              "Top " <> tInt x <> " pending (" <> tInt (p - x) <> " hidden):"


        printTaskForSummary :: [H.Task] -> Map.Map H.TaskUuid Text -> H.Task -> [Text]
        printTaskForSummary allTs prefs t =
          let idxText = maybe "" (\i -> padLeft 3 (tInt i) <> " ") (taskIndex t allTs)
          in [ idxText <> formatDescription 6 t
             , indent $ indent $ withDim printRef
             ]

          where
            printRef :: Text
            printRef = Map.findWithDefault (H.taskUuidToText (H.taskUuid t)) (H.taskUuid t) prefs

            taskIndex :: H.Task -> [H.Task] -> Maybe Int
            taskIndex task ts = (+1) <$> List.findIndex (\x -> H.taskUuid x == H.taskUuid task) (List.sortBy H.taskDisplayOrder ts)
