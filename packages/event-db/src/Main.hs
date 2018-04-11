{-# LANGUAGE OverloadedStrings          #-}

module Main (main) where

import Event
import Event.Database
import qualified Event                   as V
import qualified Database.Persist.Sqlite as S


run :: IO ()
run = do
  event01 <- V.createEvent ("fish" :: String)

  xs <- S.runSqlite "tasks.db"
    $ runSql
    $ prepareDB >> writeEvent event01 >> readEvents

  print (xs :: [V.Event String])


main :: IO ()
main = run
