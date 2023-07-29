{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}

module ProviderTests.TimedCache (testTimedCache) where

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Time
import           Leadpixel.Provider
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit


newtype TestApp a
  = TestApp { unApp :: StateT (UTCTime, Int) IO a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance Provider Int TestApp where
  provide = TestApp updateValueIfExpired


updateValueIfExpired :: StateT (UTCTime, Int) IO Int
updateValueIfExpired = do
  (storedTime, s) <- get

  let expiryTime = addUTCTime cacheDuration storedTime

  now <- lift getCurrentTime
  if now > expiryTime
    then do
      let s' = s + 1
      put (now, s')
      pure s'

    else
      pure s

  where
    cacheDuration :: NominalDiffTime
    cacheDuration = 0.5 -- seconds


runTest :: TestApp a -> IO a
runTest app = do
  x <- randomRIO (0, 100)
  t <- getCurrentTime
  evalStateT (unApp app) (t, x)


testTimedCache :: TestTree
testTimedCache = testGroup "timed cache"
  [ testCase "generates a value on first use" $ do
    p <- runTest $ provide @Int
    (p /= -1) @? "expected a value"

  , testCase "returns the generated value during the cache duration" $ do
      (p, q) <- runTest $ generateAfterDelay 100000
      (p == q) @? "expected values to match"

  , testCase "generates a new value after the cache duration" $ do
      (p, q) <- runTest $ generateAfterDelay 1000000
      (p /= q) @? "expected values not to match"
  ]

  where
    generateAfterDelay n = do
      p <- provide @Int
      liftIO $ threadDelay n
      q <- provide @Int
      pure (p, q)


