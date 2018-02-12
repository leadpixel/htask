{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified HTask as H
import Conduit
import Data.Aeson
import qualified Control.Monad.State    as State
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.List              as List
import qualified Data.UUID as UUID
import qualified Text.Show.Pretty       as Pretty
import Data.Maybe


newtype TaskApplication a = TaskApp
  { runTaskApp :: State.StateT H.Tasks IO a
  } deriving (Functor, Applicative, Monad)

instance H.HasTasks TaskApplication where
  getTasks = TaskApp $ H.getTasks
  putTasks = TaskApp . H.putTasks
  addNewTask = TaskApp . H.addNewTask
  updateExistingTask ref = TaskApp . H.updateExistingTask ref
  removeTask = TaskApp . H.removeTask

instance H.CanTime TaskApplication where
  now = TaskApp $ lift H.now

instance H.CanUuid TaskApplication where
  uuidGen = TaskApp $ lift H.uuidGen

instance H.CanStoreEvent TaskApplication where
  appendEvent
    = TaskApp
    . lift
    . BS.appendFile "tasks.txt"
    . Lazy.toStrict
    . flip mappend "\n"
    . encode


readTaskEvents :: FilePath -> IO [H.TaskEvent]
readTaskEvents p = do
  content <- readFile p
  pure $ parseLines (lines content)

  where
    parseLines :: [String] -> [H.TaskEvent]
    parseLines ls = catMaybes (fmap (decode . UTF8.fromString) ls)


runTaskApi :: [H.TaskEvent] -> TaskApplication a -> IO a
runTaskApi vs op
  = State.evalStateT
      (runTaskApp $ H.replayEventLog vs >> op)
      H.emptyTasks


main :: IO ()
main = do
  let (Just uuid) = UUID.fromString "bc32a572-8deb-48a4-b684-04e5a9cd0796"

  taskEvents <- readTaskEvents "tasks.txt"
  ts <- runTaskApi taskEvents $ do
    H.startTask uuid
    H.listTasks

  putStrLn (Pretty.ppShow (List.sortOn H.createdAt ts))
