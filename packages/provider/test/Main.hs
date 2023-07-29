module Main (main) where

import           ProviderTests.Clear
import           ProviderTests.Random
import           ProviderTests.Static
import           ProviderTests.TimedCache
import           Test.Tasty


main :: IO ()
main = defaultMain $ testGroup "Provider a m"
  [ testRandom
  , testStatic
  , testClear
  , testTimedCache
  ]
