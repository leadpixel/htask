module HTaskTests
  ( allTests
  ) where

import Test.Tasty
import HTaskTests.Replay


allTests :: TestTree
allTests = testGroup "htask tests"
  [ test_rebuild
  ]
