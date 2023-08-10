module HTask.Core.Replay
  ( applyEvent
  , foldEventLog
  ) where

import qualified Data.Map.Strict           as Map
import qualified Leadpixel.Events          as V

import           Control.Monad.Trans.State
import           Data.Foldable             (foldl')
import           Data.Map.Strict           (Map)
import           Data.Sequence             (Seq, (|>))
import           HTask.Core.Task
import           HTask.Core.TaskContainer
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
      let (success, xs') = runState (addNewTask t) xs
      if success
         then (xs', Nothing)
         else (xs', Just ev)

    (StartTask ref) -> do
      let (xs', success) = tryUpdateTask ref (setTaskStatus InProgress) xs
      if success
         then (xs', Nothing)
         else (xs', Just ev)

    (StopTask ref) -> do
      let (xs', success) = tryUpdateTask ref (setTaskStatus Pending) xs
      if success
         then (xs', Nothing)
         else (xs', Just ev)

    (CompleteTask ref) -> do
      let (xs', success) = tryUpdateTask ref (setTaskStatus Complete) xs
      if success
         then (xs', Nothing)
         else (xs', Just ev)

    (RemoveTask ref) -> do
      let (xs', success) = tryUpdateTask ref (setTaskStatus Abandoned) xs
      if success
         then (xs', Nothing)
         else (xs', Just ev)


-- TODO: remove
tryUpdateTask :: TaskUuid -> (Task -> Task) -> Map TaskUuid Task -> (Map TaskUuid Task, Bool)
tryUpdateTask ref op xs = do
  maybe (xs, False)
    (\t -> (Map.insert (taskUuid t) (op t) xs, True))
    (Map.lookup ref xs)
