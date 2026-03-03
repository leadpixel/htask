module HTask.EffectsSpec (tests) where

import           Data.Time          (Day (ModifiedJulianDay), UTCTime (..),
                                     addUTCTime)
import           HTask.Core.TestApp (runTestApp)
import           HTask.Effects
import           Test.Tasty
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "Effects"
  [ testCase "MonadTime auto-increments in TestApp" $ do
      let start = UTCTime (ModifiedJulianDay 0) 0
      (times, _) <- runTestApp start $ do
        t1 <- currentTime
        t2 <- currentTime
        pure (t1, t2)

      let (t1, t2) = times
      t1 @?= start
      t2 @?= addUTCTime 1 start

  , testCase "MonadUUID generates values in TestApp" $ do
      let start = UTCTime (ModifiedJulianDay 0) 0
      (uuids, _) <- runTestApp start $ do
        u1 <- nextUUID
        u2 <- nextUUID
        pure (u1, u2)

      let (u1, u2) = uuids
      (u1 /= u2) @? "UUIDs should not be equal"
  ]
