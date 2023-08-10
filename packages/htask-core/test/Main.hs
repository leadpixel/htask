{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Tests.Core.Add
import           Tests.Core.Complete
import           Tests.Core.List
import           Tests.Core.Remove
import           Tests.Core.Start
import           Tests.Core.Stop

import           Test.Tasty


main :: IO ()
main = defaultMain allTests


allTests :: TestTree
allTests = testGroup "htask"
  [ testAdd
  , after AllSucceed "add" testList
  , after AllSucceed "add" testStart
  , after AllSucceed "add" testStop
  , after AllSucceed "add" testComplete
  , after AllSucceed "add" testRemove
  ]
