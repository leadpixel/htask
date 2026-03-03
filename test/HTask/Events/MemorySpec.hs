{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTask.Events.MemorySpec (allTests) where

import qualified HTask.Events     as Events

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

run :: Events.MemoryEventBackend IO a -> IO a
run = Events.runMemoryBackend

fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0

createFakeEvent :: Int -> Events.Event Int
createFakeEvent x = Events.Event { Events.timestamp = fakeTime, Events.payload = x }

initialReadEmpty :: TestTree
initialReadEmpty = testCase "initial read is empty" $ do
  xs :: [Events.Event Int] <- run Events.readEvents
  assertEqual "expecting 0 items" 0 (length xs)

initialLogEmpty :: TestTree
initialLogEmpty = testCase "initial log is empty" $ do
  (_, xs) <- Events.runMemoryBackend' (Events.readEvents :: Events.MemoryEventBackend IO [Events.Event Int])
  assertEqual "expecting 0 items" Empty xs

writeSucceeds :: TestTree
writeSucceeds = testCase "writing appends to event log" $ do
  xs <- run ( Events.writeEvent (createFakeEvent 1))
  assertEqual "expecting 1 item" () xs

writeManySucceeds :: TestTree
writeManySucceeds = testCase "writing multiple appends to event log" $ do
  xs <- run ( Events.writeEvents [createFakeEvent 1, createFakeEvent 2])
  assertEqual "expecting 2 items" () xs

writeThenReadReturnsOne :: TestTree
writeThenReadReturnsOne = testCase "returns one event after writing" $ do
  xs :: [Events.Event Int] <- run $ do
    Events.writeEvent (createFakeEvent 1)
    Events.readEvents
  assertEqual "expecting 1 item" 1 (length xs)

writeThenReadReturnsTwo :: TestTree
writeThenReadReturnsTwo = testCase "returns two events after writing" $ do
  xs :: [Events.Event Int] <- run $ do
    Events.writeEvents [createFakeEvent 1, createFakeEvent 2]
    Events.readEvents
  assertEqual "expecting 2 items" 2 (length xs)

eventsRemainOrdered :: TestTree
eventsRemainOrdered = testCase "events are returned in write order" $ do
  xs :: [Events.Event Int] <- run $ do
    Events.writeEvent (createFakeEvent 1)
    Events.writeEvent (createFakeEvent 2)
    Events.readEvents
  assertEqual "expecting 2 items" [1, 2] (Events.payload <$> xs)

repeatEventsCanBeAdded :: TestTree
repeatEventsCanBeAdded = testCase "events can be added repeatedly" $ do
  xs :: [Events.Event Int] <- run $ do
    let ev = createFakeEvent 1
    Events.writeEvent ev
    Events.writeEvent ev
    Events.readEvents
  assertEqual "expecting 2 items" [1, 1] (Events.payload <$> xs)
