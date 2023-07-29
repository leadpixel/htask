{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import qualified Data.ByteString.Lazy as Lazy
import qualified Event.Backend.Memory as Memory
import qualified Events               as V

import           Data.Sequence        (Seq (..))
import           Data.Time            (Day (ModifiedJulianDay), UTCTime (..))

import           Test.Tasty
import           Test.Tasty.HUnit


main :: IO ()
main = defaultMain allTests


allTests :: TestTree
allTests = testGroup "storage::memory"
  [ initialReadEmpty
  , initialLogEmpty
  , writeAppendsToLog
  , writeAppendsMultipleToLog
  , writeThenReadReturnsOne
  , writeWriteThenReadReturnsTwo
  , eventsRemainOrdered
  , repeatEventsCanBeAdded
  ]


run :: Memory.MemoryBackend m a -> m (a, Seq Lazy.ByteString)
run = Memory.runMemoryBackend


readEvents :: (Monad m) => Memory.MemoryBackend m [V.Event Int]
readEvents = V.readEvents


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


createFakeEvent :: a -> V.Event a
createFakeEvent x = V.Event { V.timestamp = fakeTime, V.payload = x }


writeFakeEvent :: (V.HasEventSink m) => Int -> m ()
writeFakeEvent = V.writeEvent . createFakeEvent


initialReadEmpty :: TestTree
initialReadEmpty = testCase "initial read is empty" $ do
  xs <- run readEvents
  assertEqual "expecting 0 items" 0 (length $ fst xs)


initialLogEmpty :: TestTree
initialLogEmpty = testCase "initial log is empty" $ do
  xs <- run readEvents
  assertEqual "expecting 0 items" 0 (length $ snd xs)


writeAppendsToLog :: TestTree
writeAppendsToLog = testCase "writing appends to event log" $ do
  xs <- run $ writeFakeEvent 1
  assertEqual "expecting 1 item" 1 (length $ snd xs)


writeAppendsMultipleToLog :: TestTree
writeAppendsMultipleToLog = testCase "writing multiple appends to event log" $ do
  xs <- run $ mapM_ writeFakeEvent [1, 2]
  assertEqual "expecting 2 items" 2 (length $ snd xs)


writeThenReadReturnsOne :: TestTree
writeThenReadReturnsOne = testCase "returns one event after writing" $ do
  xs <- run $ do
    writeFakeEvent 1
    readEvents
  assertEqual "expecting 1 item" 1 (length $ fst xs)


writeWriteThenReadReturnsTwo :: TestTree
writeWriteThenReadReturnsTwo = testCase "returns two events after writing" $ do
  xs <- run $ do
    mapM_ writeFakeEvent [1, 2]
    readEvents
  assertEqual "expecting 2 items" 2 (length $ fst xs)


eventsRemainOrdered :: TestTree
eventsRemainOrdered = testCase "events are returned in write order" $ do
  xs <- run $ do
    mapM_ writeFakeEvent [1, 2]
    readEvents
  assertEqual "expecting 2 items" [1, 2] (V.payload <$> fst xs)


repeatEventsCanBeAdded :: TestTree
repeatEventsCanBeAdded = testCase "events can be added repeatedly" $ do
  xs <- run $ do
    writeFakeEvent 1
    writeFakeEvent 1
    readEvents
  assertEqual "expecting 2 items" [1, 1] (V.payload <$> fst xs)
