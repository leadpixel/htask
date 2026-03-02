module Main (main) where

import           Test.Tasty

import qualified HTask.CLI.CLISpec         as CLI
import qualified HTask.Core.Add            as CoreAdd
import qualified HTask.Core.Complete       as CoreComplete
import qualified HTask.Core.List           as CoreList
import qualified HTask.Core.Remove         as CoreRemove
import qualified HTask.Core.Start          as CoreStart
import qualified HTask.Core.Stop           as CoreStop
import qualified HTask.Events.CoreSpec     as EventsCore
import qualified HTask.Events.FileSpec     as EventsFile
import qualified HTask.Events.MemorySpec   as EventsMemory
import qualified HTask.Provider.Clear      as ProviderClear
import qualified HTask.Provider.Random     as ProviderRandom
import qualified HTask.Provider.Static     as ProviderStatic
import qualified HTask.Provider.TimedCache as ProviderTimedCache

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "HTask"
  [ testGroup "Provider"
      [ ProviderClear.testClear
      , ProviderRandom.testRandom
      , ProviderStatic.testStatic
      , ProviderTimedCache.testTimedCache
      ]
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
  , testGroup "CLI"
      [ CLI.tests
      ]
  ]
