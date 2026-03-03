module Main (main) where

import qualified HTask.CLI.CLISpec       as CLI
import qualified HTask.Core.Add          as CoreAdd
import qualified HTask.Core.Complete     as CoreComplete
import qualified HTask.Core.List         as CoreList
import qualified HTask.Core.Remove       as CoreRemove
import qualified HTask.Core.Start        as CoreStart
import qualified HTask.Core.Stop         as CoreStop
import qualified HTask.EffectsSpec       as Effects
import qualified HTask.Events.CoreSpec   as EventsCore
import qualified HTask.Events.FileSpec   as EventsFile
import qualified HTask.Events.MemorySpec as EventsMemory

import           Test.Tasty


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "HTask"
  [ Effects.tests
  , testGroup "Events"
      [ EventsCore.tests
      , EventsFile.allTests
      , EventsMemory.allTests
      ]
  , testGroup "Core"
      [ CoreAdd.testAdd
      , CoreComplete.testComplete
      , CoreList.testList
      , CoreRemove.testRemove
      , CoreStart.testStart
      , CoreStop.testStop
      ]
  , CLI.tests
  ]
