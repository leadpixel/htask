module Lib
    ( someFunc
    ) where

import qualified Data.UUID as UUID
import System.Random
import Control.Monad.IO.Class
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import Data.Semigroup
import Conduit
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Time


type TaskUUID = UUID.UUID
type Timestamp = UTCTime

someFunc :: IO ()
someFunc = putStrLn "someFunc"


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
  }
