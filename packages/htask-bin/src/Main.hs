{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import Data.Aeson
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Tagged
import HTask.TaskApplication
import Options.Applicative
import Options.Applicative.Builder
import qualified Control.Monad.State    as State
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.List              as List
import qualified Data.Text              as Text
import qualified Data.UUID as UUID
import qualified HTask as H
import qualified Text.Show.Pretty       as Pretty

readTaskEvents :: FilePath -> IO [H.TaskEvent]
readTaskEvents p = (parseLines . lines) <$> readFile p
  where
    parseLines :: [String] -> [H.TaskEvent]
    parseLines = catMaybes . fmap (decode . UTF8.fromString)


prepTasks :: [H.TaskEvent] -> IO [H.Task]
prepTasks vs
  = State.execStateT
      (runTaskApp $ H.replayEventLog vs)
      H.emptyTasks


runTaskApi :: [H.Task] -> TaskApplication a -> IO a
runTaskApi ts op = State.evalStateT (runTaskApp op) ts


main :: IO ()
main = do
  let (Just uuid) = UUID.fromString "bc32a572-8deb-48a4-b684-04e5a9cd0796"

  vs <- readTaskEvents "tasks.txt"
  xs <- prepTasks vs
  ts <- runTaskApi xs $ do
    -- _ <- H.startTask (Tagged uuid)
    H.listTasks

  options <- execParser
    ( info (opts <**> helper <|> pure ListAll)
    $ progDesc "f"
    )

  greet (List.sortOn H.createdAt ts) options


nicePrint :: (Show a) => a -> IO ()
nicePrint = putStrLn . Pretty.ppShow


greet :: [H.Task] -> OneAllOptions -> IO ()
greet ts ListOne = nicePrint (head ts)
greet ts ListAll = nicePrint ts
greet ts (Add tex) = runAdd ts tex
greet ts (Start ref) = runStart ts ref


runAdd :: [H.Task] -> Text.Text -> IO ()
runAdd ts tex = do
  k <- runTaskApi ts (H.addTask tex)
  print k


runStart :: [H.Task] -> Text.Text -> IO ()
runStart ts ref
  = maybe
      (print "no")
      (\v -> runTaskApi ts (H.startTask $ Tagged v) >>= print)
      (UUID.fromString $ Text.unpack ref)


data OneAllOptions
  = ListOne
  | ListAll
  | Add Text.Text
  | Start Text.Text
  deriving (Show, Read)


oneParser :: Parser OneAllOptions
oneParser = pure ListOne


allParser :: Parser OneAllOptions
allParser = pure ListAll


addParser :: Parser OneAllOptions
addParser = Add <$> argument str (metavar "DESCRIPTION")


startParser :: Parser OneAllOptions
startParser = Start <$> argument str (metavar "TASKREF")


opts :: Parser OneAllOptions
opts = hsubparser
  (  command "one" (info oneParser (progDesc "list one") )
  <> command "all"  (info allParser (progDesc "list all") )
  <> command "add"  (info addParser (progDesc "add things") )
  <> command "start"  (info startParser (progDesc "start things") )
  )
