{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified HTask as H

import Conduit
import Control.Monad.IO.Class
import Data.Semigroup
import System.Random
import Data.Foldable
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Writer as Writer
import Control.Monad
import qualified Control.Monad.State as State
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import qualified Data.Tree as Tree
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Text.Show.Pretty as Pretty


type ConcreteTaskMonad = Writer.WriterT H.EventLog (State.StateT H.Tasks IO)

instance H.CanUuid ConcreteTaskMonad where
  uuidGen = lift (lift H.uuidGen)

instance H.CanTime ConcreteTaskMonad where
  now = lift (lift H.now)


runTaskApi :: ConcreteTaskMonad a -> IO (a, H.EventLog)
runTaskApi op = do
  State.evalStateT
    (Writer.runWriterT op)
    H.emptyTasks


main :: IO ()
main = do
  putStrLn ""
  putStrLn "empty"
  ( runTaskApi $ H.listTasks ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "one task"
  ( runTaskApi $ do
      _ <- H.addTask "test"
      H.listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "two tasks"
  ( runTaskApi $ do
      _ <- H.addTask "test1"
      _ <- H.addTask "test2"
      H.listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "starting a task"
  ( runTaskApi $ do
      x <- H.addTask "test"
      case x of
        Right v -> do
          H.startTask v
      H.listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "starting root task does not start"
  ( runTaskApi $ do
      H.startTask UUID.nil
      H.listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  uuid <- UUID.nextRandom
  putStrLn "starting without task fails gracefully"
  ( runTaskApi $ do
      H.startTask uuid
      H.listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "completing a task"
  ( runTaskApi $ do
      x <- H.addTask "test"
      case x of
        Right v -> do
          H.completeTask v
      H.listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "deleting a task"
  ( runTaskApi $ do
      x <- H.addTask "test"
      case x of
        Right v -> do
          H.deleteTask v
      H.listTasks
    ) >>= putStrLn . Pretty.ppShow
