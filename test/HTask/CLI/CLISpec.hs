{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.CLISpec (tests) where

import qualified HTask.CLI.Actions    as Action
import qualified HTask.CLI.Output     as Doc
import qualified HTask.CLI.Runners    as Runner
import           HTask.CLI.TestApp    (runTestApp)

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Data.Time            (Day (ModifiedJulianDay), UTCTime (..))
import           Data.UUID            (UUID)
import qualified Data.UUID            as UUID
import           System.Directory
import           System.IO
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "htask-cli"
  [ hunitTests
  , goldenTests
  ]

fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0

fakeUUIDs :: [UUID]
fakeUUIDs = [UUID.fromWords 0 0 0 i | i <- [1..100]]

hunitTests :: TestTree
hunitTests = testGroup "Unit Tests"
  [ testCase "htask summary shows 'No current task' on empty file" $ do
      (path, h) <- openTempFile "." "cli-test-tasks"
      hClose h

      result <- runTestApp path [] fakeTime (Runner.runAction Action.Summary)
      let output = Doc.text result

      removeFile path

      let needle = "No current task"
      any (needle `Text.isInfixOf`) output
        @? ("Output should contain '" <> Text.unpack needle <> "', got: " <> show output)
  ]

goldenTests :: TestTree
goldenTests = testGroup "Golden Tests"
  [ goldenVsString
      "summary on empty file"
      "test/HTask/CLI/golden/summary_empty.golden"
      (runSummaryWith [])
  , goldenVsString
      "summary with tasks"
      "test/HTask/CLI/golden/summary_with_tasks.golden"
      (runSummaryWith
        [ Action.Add "task 1"
        , Action.Add "task 2"
        , Action.Add "task 3"
        , Action.Start "task 2"
        , Action.Complete "task 3"
        ]
      )
  ]

runSummaryWith :: [Action.Action] -> IO Lazy.ByteString
runSummaryWith actions = do
  (path, h) <- openTempFile "." "cli-test-tasks-golden"
  hClose h
  result <- runTestApp path fakeUUIDs fakeTime $ do
    mapM_ Runner.runAction actions
    Runner.runAction Action.Summary
  removeFile path
  let output = Text.unlines (Doc.text result)
  pure $ Lazy.fromStrict $ Text.encodeUtf8 output
