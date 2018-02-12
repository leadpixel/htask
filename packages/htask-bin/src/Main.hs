{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified HTask as H

import Conduit
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable
import Data.Semigroup
import System.Random
import qualified Control.Monad.Reader   as Reader
import qualified Control.Monad.State    as State
import qualified Control.Monad.Writer   as Writer
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Map               as Map
import qualified Data.Text              as Text
import qualified Data.Time              as Time
import qualified Data.Time.Clock.System as Time
import qualified Data.Tree              as Tree
import qualified Data.UUID              as UUID
import qualified Data.UUID.V4           as UUID
import qualified Text.Show.Pretty       as Pretty
import Data.Maybe


type ConcreteTaskMonad = State.StateT H.Tasks IO

instance H.CanUuid ConcreteTaskMonad where
  uuidGen = lift H.uuidGen

instance H.CanTime ConcreteTaskMonad where
  now = lift H.now

instance H.CanStoreEvent ConcreteTaskMonad where
  appendEvent
    = lift
    . BS.appendFile "tasks.txt"
    . Lazy.toStrict
    . flip mappend "\n"
    . encode


readTaskEvents :: FilePath -> IO [H.TaskEvent]
readTaskEvents p = do
  content <- readFile p
  let ts = (fmap (decode . UTF8.fromString) (lines content)) :: [Maybe H.TaskEvent]
  let vs = filter isJust ts
  pure $ (fmap fromJust vs)


applyEvents :: (Monad m, H.HasTasks m) => [H.TaskEvent] -> m H.Tasks
applyEvents vs = H.getTasks >>= H.replayEventLog vs


runTaskApi :: [H.TaskEvent] -> ConcreteTaskMonad a -> IO a
runTaskApi vs op
  = State.evalStateT
      (applyEvents vs >>= H.putTasks >> op)
      H.emptyTasks


main :: IO ()
main = do
  taskEvents <- readTaskEvents "tasks.txt"
  ts <- runTaskApi taskEvents $ do
    H.listTasks

  putStrLn (Pretty.ppShow ts)
