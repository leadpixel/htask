{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import qualified Event.Backend.File      as File
import qualified Events                  as V

import           Control.Monad           (replicateM_)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.Time               (Day (ModifiedJulianDay), UTCTime (..))

import           System.Directory
import           System.IO
import           Test.Tasty
import           Test.Tasty.HUnit


main :: IO ()
main = defaultMain allTests


allTests :: TestTree
allTests = testGroup "storage::file"
  [ initialReadEmpty
  , writeSucceeds
  , writeThenReadReturnsOne
  , eventsRemainOrdered
  , repeatEventsCanBeAdded
  , manyEvents
  , manyEvents2
  ]


run :: File.FileEventBackend IO a -> IO a
run op = do
  (path, _handle) <- openTempFile "." "file-test-tmp"
  putStrLn path
  hClose _handle
  x <- File.runFileBackend path op
  removeFile path
  pure x



readEvents :: (MonadUnliftIO m, Monad m) => File.FileEventBackend m [V.Event Int]
readEvents = V.readEvents


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


createFakeEvent :: Int -> V.Event Int
createFakeEvent x = V.Event { V.timestamp = fakeTime, V.payload = x }


initialReadEmpty :: TestTree
initialReadEmpty = testCase "initial read is empty" $ do
  xs <- run readEvents
  assertEqual "expecting 0 items" 0 (length xs)


writeSucceeds :: TestTree
writeSucceeds = testCase "writing appends to event log" $ do
  xs <- run ( V.writeEvent (createFakeEvent 1))
  assertEqual "expecting 1 item" () xs


writeThenReadReturnsOne :: TestTree
writeThenReadReturnsOne = testCase "returns one event after writing" $ do
  xs <- run $ do
    V.writeEvent (createFakeEvent 1)
    readEvents
  assertEqual "expecting 1 item" 1 (length xs)


eventsRemainOrdered :: TestTree
eventsRemainOrdered = testCase "events are returned in write order" $ do
  xs <- run $ do
    V.writeEvent (createFakeEvent 1)
    V.writeEvent (createFakeEvent 2)
    readEvents
  assertEqual "expecting 2 items" [1, 2] (V.payload <$> xs)


repeatEventsCanBeAdded :: TestTree
repeatEventsCanBeAdded = testCase "events can be added repeatedly" $ do
  xs <- run $ do
    let ev = createFakeEvent 1
    V.writeEvent ev
    V.writeEvent ev
    readEvents
  assertEqual "expecting 2 items" [1, 1] (V.payload <$> xs)


manyEvents :: TestTree
manyEvents = testCase "repeated writing (one at a time)" $ do
  let ev = createFakeEvent 1
  xs <- run $ do
    replicateM_ 100000 ( V.writeEvent ev)
    readEvents
  assertEqual "expecting 100000 items" 100000 (length xs)


manyEvents2 :: TestTree
manyEvents2 = testCase "repeated writing (batch)" $ do
  let ev = createFakeEvent 1
  let evs = replicate 100000 ev
  xs <- run $ do
    mapM_ V.writeEvent evs
    readEvents
  assertEqual "expecting 100000 items" 100000 (length xs)
