module Main (main) where

import           Test.Tasty
import           Tests.Provider.Clear
import           Tests.Provider.Random
import           Tests.Provider.Static
import           Tests.Provider.TimedCache


main :: IO ()
main = defaultMain $ testGroup "Provider a m"
  [ testRandom
  , testStatic
  , testClear
  , testTimedCache
  ]
