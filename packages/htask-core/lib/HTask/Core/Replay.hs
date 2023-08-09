module HTask.Core.Replay
  ( applyEvent
  , foldEventLog
  ) where

import qualified Data.Map.Strict      as Map
import qualified Leadpixel.Events     as V

import           Data.Foldable        (foldl')
import           Data.Map.Strict      (Map)
import           Data.Sequence        (Seq, (|>))
import           HTask.Core.Task
import           HTask.Core.TaskEvent


foldEventLog
  :: (Foldable f)
  => f TaskEvent -> (Map TaskUuid Task, Seq TaskEvent)
foldEventLog = foldl' run (mempty, mempty)

  where
    run (accMap, accSeq) x = do
      let (xs, mt) = applyEvent accMap x

      let accSeq' = maybe accSeq (accSeq |>) mt

      (xs, accSeq')




applyEvent :: Map TaskUuid Task -> TaskEvent -> (Map TaskUuid Task, Maybe TaskEvent)
applyEvent xs ev =
  case V.payload ev of
    (AddTask ref text) -> do
      let t = Task ref text (V.timestamp ev) Pending
      let (xs', p) = tryInsertTask t xs
      if p
         then (xs', Nothing)
         else (xs', Just ev)

    (StartTask ref) -> do
      let (xs', p) = tryUpdateTask ref (setTaskStatus InProgress) xs
      if p
         then (xs', Nothing)
         else (xs', Just ev)

    (StopTask ref) -> do
      let (xs', p) = tryUpdateTask ref (setTaskStatus Pending) xs
      if p
         then (xs', Nothing)
         else (xs', Just ev)

    (CompleteTask ref) -> do
      let (xs', p) = tryUpdateTask ref (setTaskStatus Complete) xs
      if p
         then (xs', Nothing)
         else (xs', Just ev)

    (RemoveTask ref) -> do
      let (xs', p) = tryUpdateTask ref (setTaskStatus Abandoned) xs
      if p
         then (xs', Nothing)
         else (xs', Just ev)


tryInsertTask :: Task -> Map TaskUuid Task -> (Map TaskUuid Task, Bool)
tryInsertTask t xs =
  if Map.member (taskUuid t) xs
    then (xs, False)
    else (Map.insert (taskUuid t) t xs, True)


tryUpdateTask :: TaskUuid -> (Task -> Task) -> Map TaskUuid Task -> (Map TaskUuid Task, Bool)
tryUpdateTask ref op xs = do
  maybe (xs, False)
    (\t -> (Map.insert (taskUuid t) (op t) xs, True))
    (Map.lookup ref xs)
