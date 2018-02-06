{-# LANGUAGE OverloadedStrings #-}

   module Lib
    ( someFunc
    ) where

import qualified Data.UUID as UUID
import System.Random
import Control.Monad.IO.Class
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Data.Semigroup
import Data.Monoid
import Conduit
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Time
import Data.Tree


type TaskUUID = UUID.UUID
type Timestamp = UTCTime


someFunc :: IO ()
someFunc = runConduit $ do
  t <- sourceFile "tasks.txt"
  sourceFile putStrLn "someFunc"


data TaskEvent
  = TaskAdd
  | TaskComplete
  | TaskAbandon
  | TaskAssign



data TaskStatus
  = Pending
  | Started
  | Blocked
  | Complete
  | Aborted
  deriving (Show, Eq)


data Task = Task
  { taskUuid :: TaskUUID
  , taskDescription :: T.Text
  , taskCreatedAt :: Timestamp
  , taskStatus :: TaskStatus
  , taskParent :: TaskUUID
  }


type TaskTree = Tree Task
