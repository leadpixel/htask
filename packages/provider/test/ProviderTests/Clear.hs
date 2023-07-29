{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}

module ProviderTests.Clear (testClear) where

import qualified Data.UUID.V4              as UUID

import           Control.Monad.IO.Class
import           Control.Monad.State       (MonadState (..))
import           Control.Monad.Trans.State (StateT, evalStateT)
import           Data.Functor
import           Data.UUID                 (UUID)
import           Leadpixel.Provider
import           System.Random
import           Test.Tasty
import           Test.Tasty.HUnit


newtype TestApp a
  = TestApp { unApp :: StateT (Maybe Int) IO a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance Provider Int TestApp where
  provide = TestApp updateValue

instance Provider UUID TestApp where
  provide = TestApp $ liftIO UUID.nextRandom


updateValue :: (MonadIO m, MonadState (Maybe Int) m) => m Int
updateValue =
  get >>= maybe onNothing onJust

  where
    onJust v = put Nothing $> v

    onNothing = do
      x <- liftIO $ randomRIO (0, 100 :: Int)
      put (Just x)
      pure x


runTestApp :: TestApp a -> IO a
runTestApp app =
  evalStateT (unApp app) Nothing


testClear :: TestTree
testClear = testGroup "clears value on access"
  [ testCase "generates a value on first use" $ do
    x <- runTestApp $ provide @Int
    (x >= 0) @? "expected integer value between 0 and 100"
    (x <= 100) @? "expected integer value between 0 and 100"

  , testCase "returns the generated value on second call" $ do
      (x, y) <- runTestApp $ (,) <$> provide @Int <*> provide @Int
      (x == y) @? "expected values to be equal"

  , testCase "generates a new value on third call" $ do
      (x, y, z) <- runTestApp $ (,,)
        <$> provide @Int
        <*> provide @Int
        <*> provide @Int

      (x == y) @? "expected values to be equal"
      (x /= z) @? "expected values to not be equal"

  , testCase "generates a new value after empty access" $ do
      (a, b, c, d) <- runTestApp $ (,,,)
        <$> provide @Int
        <*> provide @Int
        <*> provide @Int
        <*> provide @Int

      (a == b) @? "expected values to be equal"
      (a /= c) @? "expected values to not be equal"
      (c == d) @? "expected values to be equal"
  ]
