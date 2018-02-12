{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import qualified HTask as H
import HTask.TaskApplication
import Data.Tagged
import Data.Aeson
import qualified Control.Monad.State    as State
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.List              as List
import qualified Data.UUID as UUID
import qualified Text.Show.Pretty       as Pretty
import Data.Maybe


readTaskEvents :: FilePath -> IO [H.TaskEvent]
readTaskEvents p = (parseLines . lines) <$> readFile p
  where
    parseLines :: [String] -> [H.TaskEvent]
    parseLines = catMaybes . fmap (decode . UTF8.fromString)


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
    _ <- H.startTask (Tagged uuid)
    H.listTasks

  putStrLn (Pretty.ppShow (List.sortOn H.createdAt ts))
