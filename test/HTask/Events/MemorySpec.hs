{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTask.Events.MemorySpec (allTests) where

import qualified HTask.Events     as Memory
import qualified HTask.Events     as V

import           Data.Sequence    (Seq (..))
import           Data.Time        (Day (ModifiedJulianDay), UTCTime (..))
import           Test.Tasty
import           Test.Tasty.HUnit


allTests :: TestTree
allTests = testGroup "storage::memory"
  [ initialReadEmpty
  , initialLogEmpty
  , writeSucceeds
  , writeManySucceeds
  , writeThenReadReturnsOne
  , writeThenReadReturnsTwo
  , eventsRemainOrdered
  , repeatEventsCanBeAdded
  ]


run :: Memory.MemoryEventBackend IO a -> IO a
run = Memory.runMemoryBackend


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


createFakeEvent :: Int -> V.Event Int
createFakeEvent x = V.Event { V.timestamp = fakeTime, V.payload = x }


initialReadEmpty :: TestTree
initialReadEmpty = testCase "initial read is empty" $ do
  xs :: [V.Event Int] <- run V.readEvents
  assertEqual "expecting 0 items" 0 (length xs)


initialLogEmpty :: TestTree
initialLogEmpty = testCase "initial log is empty" $ do
  (_, xs) <- Memory.runMemoryBackend' (V.readEvents :: Memory.MemoryEventBackend IO [V.Event Int])
  assertEqual "expecting 0 items" Empty xs


writeSucceeds :: TestTree
writeSucceeds = testCase "writing appends to event log" $ do
  xs <- run ( V.writeEvent (createFakeEvent 1))
  assertEqual "expecting 1 item" () xs


writeManySucceeds :: TestTree
writeManySucceeds = testCase "writing multiple appends to event log" $ do
  xs <- run ( V.writeEvents [createFakeEvent 1, createFakeEvent 2])
  assertEqual "expecting 2 items" () xs


writeThenReadReturnsOne :: TestTree
writeThenReadReturnsOne = testCase "returns one event after writing" $ do
  xs :: [V.Event Int] <- run $ do
    V.writeEvent (createFakeEvent 1)
    V.readEvents
  assertEqual "expecting 1 item" 1 (length xs)


writeThenReadReturnsTwo :: TestTree
writeThenReadReturnsTwo = testCase "returns two events after writing" $ do
  xs :: [V.Event Int] <- run $ do
    V.writeEvents [createFakeEvent 1, createFakeEvent 2]
    V.readEvents
  assertEqual "expecting 2 items" 2 (length xs)


eventsRemainOrdered :: TestTree
eventsRemainOrdered = testCase "events are returned in write order" $ do
  xs :: [V.Event Int] <- run $ do
    V.writeEvent (createFakeEvent 1)
    V.writeEvent (createFakeEvent 2)
    V.readEvents
  assertEqual "expecting 2 items" [1, 2] (V.payload <$> xs)


repeatEventsCanBeAdded :: TestTree
repeatEventsCanBeAdded = testCase "events can be added repeatedly" $ do
  xs :: [V.Event Int] <- run $ do
    let ev = createFakeEvent 1
    V.writeEvent ev
    V.writeEvent ev
    V.readEvents
  assertEqual "expecting 2 items" [1, 1] (V.payload <$> xs)
