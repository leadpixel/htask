{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}

module Tests.Provider.Random (testRandom) where

import           Control.Monad.Reader
import           Leadpixel.Provider
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit


newtype TestApp a
  = TestApp { unApp :: IO a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance Provider Int TestApp where
  provide = randomRIO (0, 100)

instance Provider Bool TestApp where
  provide = randomIO


testRandom :: TestTree
testRandom = testGroup "random"
  [ testCase "generates a new value" $ do
      k <- unApp $ provide @Int
      k >= 2 @? "expected value to be greater than 2"
      k <= 200 @? "expected value to be less than 200"

  , testCase "can provide many types (avoids fundep issue)" $ do
      _ <- unApp $ do
        n <- provide @Int
        p <- provide @Bool
        pure (n, p)

      -- no assertion required; this is tested by compilation
      pure ()
  ]
