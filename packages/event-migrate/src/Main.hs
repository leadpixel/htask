{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Trans.Resource as Rt
import qualified Data.Aeson                   as A
import qualified Data.ByteString              as BS
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Combinators     as Cx
import qualified Data.List                    as List
import qualified Data.Maybe                   as M
import qualified Database.Persist.Sqlite as S

import Event
import Event.Database
import Data.Conduit (($$), ($=))


type SomeEvent = Event A.Value


main :: IO ()
main = do
  print ("Event Migration (file -> db)" :: String)

  xs <- sortEvents <$> conduitReadEvents ".tasks"
  dbWriteEvents xs


conduitReadEvents :: FilePath -> IO [Maybe SomeEvent]
conduitReadEvents file = do
  content <- Rt.runResourceT
    $ Cx.sourceFile file
    $= splitLines
    $$ Cx.sinkList

  pure (A.decodeStrict <$> content)


splitLines :: C.Conduit BS.ByteString (Rt.ResourceT IO) BS.ByteString
splitLines = Cx.linesUnboundedAscii


sortEvents :: [Maybe (Event a)] -> [Event a]
sortEvents = List.sortOn timestamp . M.catMaybes


dbWriteEvents :: (A.ToJSON a) => [Event a] -> IO ()
dbWriteEvents xs
  = S.runSqlite "tasks.db" $ do
      prepareDB
      mapM_ writeEvent xs
