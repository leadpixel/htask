{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners
  ( runCommand
  ) where

import qualified HTask as H
import HTask.Actions
import Control.Monad
import Data.Semigroup ((<>))
import Data.Tagged
import Data.List
import HTask.TaskApplication
import qualified Data.Text              as Text
import qualified Data.UUID as UUID


taskDisplayOrder :: H.Task -> H.Task -> Ordering
taskDisplayOrder a b = statusDisplayOrder (H.status a) (H.status b)
  where
    statusDisplayOrder :: H.TaskStatus -> H.TaskStatus -> Ordering
    statusDisplayOrder   _             H.InProgress   =   GT
    statusDisplayOrder   H.Complete    H.Pending      =   GT
    statusDisplayOrder   H.Abandoned   H.Pending      =   GT
    statusDisplayOrder   H.Abandoned   H.Complete     =   GT
    statusDisplayOrder   _             H.Abandoned    =   LT
    statusDisplayOrder   _             _              =   EQ


runCommand :: Action -> [H.Task] -> IO ()
runCommand List           = runList
runCommand (Add tex)      = runAdd tex
runCommand (Start ref)    = runStart ref
runCommand (Complete ref) = runComplete ref
runCommand (Remove ref)   = runRemove ref


runList :: [H.Task] -> IO ()
runList ts = mapM_ (mapM_ nicePrint) (groupBy sameStatus $ sortBy taskDisplayOrder ts)
  where
    sameStatus :: H.Task -> H.Task -> Bool
    sameStatus a b = H.status a == H.status b


nicePrint :: H.Task -> IO ()
nicePrint t = putStrLn
  (  show (untag $ H.taskRef t)
  <> " "
  <> symbolFor t
  <> " "
  <> showDescription t
  )


symbolFor :: H.Task -> String
symbolFor t
  = case H.status t of
      H.Pending    -> "."
      H.InProgress -> ">"
      H.Complete   -> "✓"
      H.Abandoned  -> "-"


showDescription :: H.Task -> String
showDescription t
  =  statusColor (Just $ H.status t)
  <> Text.unpack (H.description t)
  <> statusColor Nothing


statusColor :: Maybe H.TaskStatus -> String
statusColor Nothing             = "\x1b[0m"  -- Clear
statusColor (Just H.Pending)    = "\x1b[34m" -- Blue
statusColor (Just H.InProgress) = "\x1b[33m" -- Yellow
statusColor (Just H.Complete)   = "\x1b[32m" -- Green
statusColor (Just H.Abandoned)  = "\x1b[31m" -- Red


justOne :: [a] -> Maybe a
justOne [x] = Just x
justOne _ = Nothing


findMatchingUUIDs :: Text.Text -> [H.Task] -> [Text.Text]
findMatchingUUIDs ref ts = filter (ref `Text.isPrefixOf`) (fmap taskRefToText ts)
  where
    taskRefToText :: H.Task -> Text.Text
    taskRefToText = UUID.toText . untag . H.taskRef


runAdd :: Text.Text -> [H.Task] -> IO ()
runAdd tex ts = do
  k <- runTask (H.addTask tex) ts
  print k


runWithMatch :: (Show a) => (H.TaskRef -> TaskApplication a) -> Text.Text -> [H.Task] -> IO ()
runWithMatch f ref ts
  = maybe
      (print $ "did not found unique match for: " <> ref)
      (\v -> runTask (f $ Tagged v) ts >>= print)
      (justOne (findMatchingUUIDs ref ts) >>= UUID.fromText)


runStart :: Text.Text -> [H.Task] -> IO ()
runStart ref ts = runWithMatch H.startTask ref ts


runComplete :: Text.Text -> [H.Task] -> IO ()
runComplete ref ts = runWithMatch H.completeTask ref ts


runRemove :: Text.Text -> [H.Task] -> IO ()
runRemove ref ts = runWithMatch H.deleteTask ref ts
