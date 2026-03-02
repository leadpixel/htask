{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified HTask.CLI.Actions         as Action
import qualified HTask.CLI.App             as App
import qualified HTask.CLI.Output.Document as Doc
import qualified HTask.CLI.Runners         as Runner

import qualified Data.Text                 as Text
import           System.Directory
import           System.IO
import           Test.Tasty
import           Test.Tasty.HUnit


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "htask-cli"
  [ testCase "htask summary shows 'No current task' on empty file" $ do
      (path, h) <- openTempFile "." "cli-test-tasks"
      hClose h

      result <- App.runApp path (Runner.runAction Action.Summary)
      let output = Doc.text result

      removeFile path

      let needle = "No current task"
      any (needle `Text.isInfixOf`) output
        @? ("Output should contain '" <> Text.unpack needle <> "', got: " <> show output)
  ]
