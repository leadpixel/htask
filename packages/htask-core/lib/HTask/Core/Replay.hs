module HTask.Core.Replay
  ( applyEvent
  , foldEventLog
  ) where

import qualified Data.Map.Strict      as Map
import qualified Data.Sequence        as Seq
import qualified Leadpixel.Events     as V

import           Data.Foldable        (foldl')
import           Data.Map.Strict      (Map)
import           Data.Sequence        (Seq)
import           HTask.Core.Task
import           HTask.Core.TaskEvent


foldEventLog
  :: (Foldable f)
  => f TaskEvent -> Seq Task
foldEventLog = Seq.fromList . Map.elems . foldl' applyEvent mempty


applyEvent :: Map TaskUuid Task -> TaskEvent -> Map TaskUuid Task
applyEvent xs ev =
  case V.payload ev of
    (AddTask ref text) -> do
      let t = Task ref text (V.timestamp ev) Pending
      addNewTask t xs

    (StartTask ref) ->
      updateExistingTask ref (setTaskStatus InProgress) xs

    (StopTask ref) ->
      updateExistingTask ref (setTaskStatus Pending) xs

    (CompleteTask ref) ->
      updateExistingTask ref (setTaskStatus Complete) xs

    (RemoveTask ref) ->
      updateExistingTask ref (setTaskStatus Abandoned) xs


addNewTask :: Task -> Map TaskUuid Task -> Map TaskUuid Task
addNewTask t xs =
    if Map.member (taskUuid t) xs
        then xs
        else Map.insert (taskUuid t) t xs


updateExistingTask :: TaskUuid -> (Task -> Task) -> Map TaskUuid Task -> Map TaskUuid Task
updateExistingTask ref op xs = do
  maybe xs
    (\t -> do
      let t' = op t
      Map.insert (taskUuid t) t' xs
      )
    (Map.lookup ref xs)
