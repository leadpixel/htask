{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified HTask as H
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson as A
import Data.Maybe
import Data.List


type SomeEvent = H.Event Int


main :: IO ()
main = do
  let file = ".tasks"

  content <- readContent file

  let es = parseEvents content
  let ps = sortByTimestamp es
  let qs = convertToJSON ps

  overwriteFile ".tasks2" qs



readContent :: FilePath -> IO [String]
readContent p = lines <$> readFile p


parseEvents :: [String] -> [SomeEvent]
parseEvents = catMaybes . fmap (decode . UTF8.fromString)


sortByTimestamp :: [SomeEvent] -> [SomeEvent]
sortByTimestamp = sortOn H.timestamp


convertToJSON :: [SomeEvent] -> [LBS.ByteString]
convertToJSON = fmap A.encode


overwriteFile :: FilePath -> [LBS.ByteString] -> IO ()
overwriteFile p ts = mapM_ (LBS.appendFile p) ts
