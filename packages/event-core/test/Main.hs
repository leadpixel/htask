{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import qualified Data.Aeson                     as Aeson
import qualified Leadpixel.Events               as V

import           Test.QuickCheck
import           Test.QuickCheck.Instances.Time ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

instance (Arbitrary a) => Arbitrary (V.Event a) where
  arbitrary = V.Event <$> arbitrary <*> arbitrary

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "leadpixel-events"
  [ jsonRoundtrip
  ]

jsonRoundtrip :: TestTree
jsonRoundtrip = testProperty "Event JSON round-trip" $
  \(ev :: V.Event Int) ->
    Aeson.decode (Aeson.encode ev) == Just ev
