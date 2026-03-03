{-# LANGUAGE OverloadedStrings #-}

module HTask.CLI.CLISpec (tests) where

import qualified HTask.CLI.Actions    as Actions
import qualified HTask.CLI.Options    as Options
import qualified HTask.CLI.Output     as Output
import qualified HTask.CLI.Runners    as Runners
import           HTask.CLI.TestApp    (runTestApp)

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Encoding
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

mockOptions :: Actions.Action -> FilePath -> Bool -> Options.Options
mockOptions act path useJson = Options.Options
  { Options.action = act
  , Options.taskfile = path
  , Options.useJson = useJson
  }

hunitTests :: TestTree
hunitTests = testGroup "Unit Tests"
  [ testCase "htask summary shows 'No current task' on empty file" $ do
      (path, h) <- openTempFile "." "cli-test-tasks"
      hClose h

      result <- runTestApp path [] fakeTime (Runners.runAction (mockOptions Actions.Summary path False))
      let output = Output.text result

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
      (runSummaryWith False [])
  , goldenVsString
      "summary with tasks"
      "test/HTask/CLI/golden/summary_with_tasks.golden"
      (runSummaryWith False
        [ Actions.Add "task 1"
        , Actions.Add "task 2"
        , Actions.Add "task 3"
        , Actions.Start "task 2"
        , Actions.Complete "task 3"
        ]
      )
  , goldenVsString
      "summary with tasks (json)"
      "test/HTask/CLI/golden/summary_with_tasks_json.golden"
      (runSummaryWith True
        [ Actions.Add "task 1"
        , Actions.Add "task 2"
        , Actions.Add "task 3"
        , Actions.Start "task 2"
        , Actions.Complete "task 3"
        ]
      )
  ]

runSummaryWith :: Bool -> [Actions.Action] -> IO Lazy.ByteString
runSummaryWith useJson actions = do
  (path, h) <- openTempFile "." "cli-test-tasks-golden"
  hClose h
  result <- runTestApp path fakeUUIDs fakeTime $ do
    mapM_ (Runners.runAction . \a -> mockOptions a path False) actions
    Runners.runAction (mockOptions Actions.Summary path useJson)
  removeFile path
  let output = Text.unlines (Output.text result)
  pure $ Lazy.fromStrict $ Encoding.encodeUtf8 output
