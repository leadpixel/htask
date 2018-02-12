{-# LANGUAGE FlexibleInstances #-}

module HTask.TaskContainer
  where

import Data.Maybe
import Data.List
import HTask.Capabilities
import HTask.Event
import HTask.Task
import Conduit
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Semigroup
import Data.Tree
import System.Random
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Writer as Writer
import qualified Control.Monad.State as State
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Tree as Tree
import qualified Data.UUID as UUID


type Tasks = [Task]


class HasTasks m where
  getTasks :: m Tasks
  putTasks :: Tasks -> m ()

  addNewTask :: Task -> m Bool
  updateExistingTask :: TaskRef -> (Task -> Task) -> m Bool
  removeTask :: TaskRef -> m Bool



instance (Monad m) => HasTasks (State.StateT Tasks m) where
  getTasks = State.get
  putTasks = State.put

  addNewTask t = do
    ts <- getTasks
    let p = findTask ts (taskRef t)
    if isNothing p
       then do
         putTasks (t : ts)
         pure True
       else pure False

  updateExistingTask ref op = do
    ts <- getTasks
    maybe
      (pure False)
      (\t -> do
        let k = op t
        putTasks (k : (removeRef ts ref))
        pure True)
      (findTask ts ref)

  removeTask ref = do
    ts <- getTasks
    let p = findTask ts ref
    maybe
      (pure False)
      (\t -> do
        putTasks (removeRef ts ref)
        pure True)
      p


instance (Monad m, MonadTrans t) => HasTasks (t (State.StateT Tasks m)) where
  getTasks = lift getTasks
  putTasks = lift . putTasks
  addNewTask = lift . addNewTask
  updateExistingTask ref = lift . updateExistingTask ref
  removeTask = lift . removeTask



removeRef :: Tasks -> TaskRef -> Tasks
removeRef ts ref = filter (\k -> taskRef k /= ref) ts


-- instance (Monad m, MonadTrans t) => HasTasks (t (State.StateT Tasks m)) where
--   getTasks = lift getTasks
--   putTasks = lift . putTasks


-- class (Monad m) => TaskContainer m where
--   emptyTasks :: m Task
--   insertTask :: Task -> m Task
--   updateTask :: Task -> m Task
--   removeTask :: TaskRef -> m Task


-- instance TaskContainer [Task] where
--   emptyTasks = []
--   insertTask t = [t]
--   updateTask t = [t]
--   removeTask r = []

--   insertTask t = do
--     ts <- getTasks
--     let p = isNothing $ findTask ts (taskRef t)
--     if p
--        then putTasks (t : ts)
--        else pure ts

--   updateTask t = do
--     ts <- getTasks
--     let p = isJust $ findTask ts (taskRef t)
--     if p
--        then putTasks (t : (ts `reject` \k -> k == t))
--        else pure ts

--   removeTask t = do
--     ts <- getTasks
--     putTasks (ts `reject` \k -> taskRef k == t)
--     ts' <- getTasks
--     pure ts'




emptyTasks :: Tasks
emptyTasks = mempty


findTask :: Tasks -> TaskRef -> Maybe Task
findTask ts ref = find matchesRef ts
  where
    matchesRef t = taskRef t == ref




insertUpdate :: Tasks -> Task -> Tasks
insertUpdate ts t = insertTask t (removeTask (taskRef t) ts)
  where
    removeTask :: TaskRef -> Tasks -> Tasks
    removeTask ref ts = rejectT (\t -> taskRef t /= ref) ts

    insertTask :: Task -> Tasks -> Tasks
    insertTask t ts = t:ts


rejectT :: (Task -> Bool) -> Tasks -> Tasks
rejectT f = filterT (not . f)


filterT :: (Task -> Bool) -> Tasks -> Tasks
filterT = filter


findParent :: Tasks -> Task -> Maybe Task
findParent r t = Nothing
