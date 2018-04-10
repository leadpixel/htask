{-# LANGUAGE OverloadedStrings          #-}

module Main (main) where

import Event.Database
import qualified Event                   as V
import qualified Database.Persist.Sqlite as S


run :: IO ()
run = do
  event01 <- V.createEvent ("fish" :: String)

  S.runSqlite "tasks.db" prepareDB

  S.runSqlite "tasks.db" $ sqliteWriteEvent event01

  xs <- S.runSqlite "tasks.db" sqliteReadAll
  print (xs :: [V.Event String])


main :: IO ()
main = run
