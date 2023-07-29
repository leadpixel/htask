{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}

module ProviderTests.Static (testStatic) where

import           Control.Monad.Trans.Reader
import           Leadpixel.Provider
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit


newtype TestApp a
  = TestApp { unApp :: ReaderT Int IO a }
  deriving (Applicative, Functor, Monad, Provider Int)


runTest :: TestApp a -> Int -> IO a
runTest app =
  runReaderT (unApp app)


testStatic :: TestTree
testStatic = testGroup "static value"
  [ testCase "accesses a stored value" $ do
      v <- randomRIO (0, 100)
      p <- runTest (provide @Int) v
      (v == p) @? "expected input value"

  , testCase "returns the same value" $ do
      v <- randomRIO (0, 100)
      k <- runTest provideTwice v
      k @? "expected values to be equal"
  ]

  where
    provideTwice :: (Monad m, Provider Int m) => m Bool
    provideTwice  = (==) <$> provide @Int <*> provide @Int
