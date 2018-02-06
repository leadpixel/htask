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


main :: IO ()
main = do
  putStrLn "empty"
  buildEventTree [] >>= print . displayTree

  putStrLn "\none task"
  buildEventTree [ TaskAdd "test" ] >>= print

  putStrLn "\ntwo tasks"
  buildEventTree [ TaskAdd "test", TaskAdd "test2" ] >>= print

  putStrLn "\nstarting a task"
  ( runEventTree $ do
      x <- addTask "test"
      lift $ print x
      startTask x
      listTasks
    ) >>= print

  putStrLn "\nstarting root task does not start"
  ( runEventTree $ do
      startTask UUID.nil
      listTasks
    ) >>= print

  uuid <- UUID.nextRandom
  putStrLn "\nstarting without task fails gracefully"
  ( runEventTree $ do
      startTask uuid
      listTasks
    ) >>= print
