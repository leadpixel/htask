{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HTask.Events.FileSpec (allTests) where

import qualified HTask.Events            as Events

import           Control.Exception       (bracket)
import           Control.Monad           (replicateM_)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.List               as List
import           Data.Time               (Day (ModifiedJulianDay), UTCTime (..))
import           GHC.IO.Handle           (hDuplicate, hDuplicateTo)

import           System.Directory
import qualified System.IO               as Sys
import           System.IO
import           Test.Tasty
import           Test.Tasty.HUnit

allTests :: TestTree
allTests = testGroup "storage::file"
  [ initialReadEmpty
  , writeSucceeds
  , writeThenReadReturnsOne
  , eventsRemainOrdered
  , repeatEventsCanBeAdded
  , manyEvents
  , manyEvents2
  , detectsDecodingErrors
  ]

detectsDecodingErrors :: TestTree
detectsDecodingErrors = testCase "reports decoding errors to stderr" $ do
  (path, h) <- openTempFile "." "file-test-tmp-error"
  hPutStrLn h "invalid json"
  hClose h

  output <- captureStderr $ Events.runFileBackend path (Events.readEvents :: Events.FileEventBackend IO [Events.Event Int])

  removeFile path

  ("Warning: Failed to decode event on line 1" `List.isInfixOf` output)
    @? ("Warning should be printed to stderr, got: " <> output)

captureStderr :: IO a -> IO String
captureStderr action = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "stderr-capture"
  hClose h
  bracket
    (do
      stderrDup <- hDuplicate stderr
      Sys.withFile path Sys.WriteMode $ \h' -> hDuplicateTo h' stderr
      pure stderrDup
    )
    (\stderrDup -> do
      hDuplicateTo stderrDup stderr
      hClose stderrDup
      removeFile path
    )
    (\_ -> do
      _ <- action
      readFile path
    )

run :: Events.FileEventBackend IO a -> IO a
run op = do
  (path, _handle) <- openTempFile "." "file-test-tmp"
  putStrLn path
  hClose _handle
  x <- Events.runFileBackend path op
  removeFile path
  pure x

readEvents :: (MonadUnliftIO m, Monad m) => Events.FileEventBackend m [Events.Event Int]
readEvents = Events.readEvents

fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0

createFakeEvent :: Int -> Events.Event Int
createFakeEvent x = Events.Event { Events.timestamp = fakeTime, Events.payload = x }

initialReadEmpty :: TestTree
initialReadEmpty = testCase "initial read is empty" $ do
  xs <- run readEvents
  assertEqual "expecting 0 items" 0 (length xs)

writeSucceeds :: TestTree
writeSucceeds = testCase "writing appends to event log" $ do
  xs <- run ( Events.writeEvent (createFakeEvent 1))
  assertEqual "expecting 1 item" () xs

writeThenReadReturnsOne :: TestTree
writeThenReadReturnsOne = testCase "returns one event after writing" $ do
  xs <- run $ do
    Events.writeEvent (createFakeEvent 1)
    readEvents
  assertEqual "expecting 1 item" 1 (length xs)

eventsRemainOrdered :: TestTree
eventsRemainOrdered = testCase "events are returned in write order" $ do
  xs <- run $ do
    Events.writeEvent (createFakeEvent 1)
    Events.writeEvent (createFakeEvent 2)
    readEvents
  assertEqual "expecting 2 items" [1, 2] (Events.payload <$> xs)

repeatEventsCanBeAdded :: TestTree
repeatEventsCanBeAdded = testCase "events can be added repeatedly" $ do
  xs <- run $ do
    let ev = createFakeEvent 1
    Events.writeEvent ev
    Events.writeEvent ev
    readEvents
  assertEqual "expecting 2 items" [1, 1] (Events.payload <$> xs)

manyEvents :: TestTree
manyEvents = testCase "repeated writing (one at a time)" $ do
  let ev = createFakeEvent 1
  xs <- run $ do
    replicateM_ 100000 ( Events.writeEvent ev)
    readEvents
  assertEqual "expecting 100000 items" 100000 (length xs)

manyEvents2 :: TestTree
manyEvents2 = testCase "repeated writing (batch)" $ do
  let ev = createFakeEvent 1
  let evs = replicate 100000 ev
  xs <- run $ do
    mapM_ Events.writeEvent evs
    readEvents
  assertEqual "expecting 100000 items" 100000 (length xs)
