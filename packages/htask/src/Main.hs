{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import API

import Conduit
import Control.Monad.IO.Class
import Data.Semigroup
import System.Random
import Data.Foldable
import qualified Control.Monad.Reader as Reader
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


main :: IO ()
main = do
  putStrLn ""
  putStrLn "empty"
  ( runTaskApi $ listTasks ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "one task"
  ( runTaskApi $ do
      _ <- addTask "test"
      listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "two tasks"
  ( runTaskApi $ do
      _ <- addTask "test1"
      _ <- addTask "test2"
      listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "starting a task"
  ( runTaskApi $ do
      x <- addTask "test"
      case x of
        Right v -> do
          startTask v
      listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "starting root task does not start"
  ( runTaskApi $ do
      startTask UUID.nil
      listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  uuid <- UUID.nextRandom
  putStrLn "starting without task fails gracefully"
  ( runTaskApi $ do
      startTask uuid
      listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "completing a task"
  ( runTaskApi $ do
      x <- addTask "test"
      case x of
        Right v -> do
          completeTask v
      listTasks
    ) >>= putStrLn . Pretty.ppShow

  putStrLn ""
  putStrLn "deleting a task"
  ( runTaskApi $ do
      x <- addTask "test"
      case x of
        Right v -> do
          deleteTask v
      listTasks
    ) >>= putStrLn . Pretty.ppShow
