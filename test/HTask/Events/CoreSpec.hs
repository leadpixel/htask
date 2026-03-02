{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HTask.Events.CoreSpec (tests) where

import qualified Data.Aeson                     as Aeson
import qualified HTask.Events                   as V

import           Test.QuickCheck
import           Test.QuickCheck.Instances.Time ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

instance (Arbitrary a) => Arbitrary (V.Event a) where
  arbitrary = V.Event <$> arbitrary <*> arbitrary

tests :: TestTree
tests = testGroup "leadpixel-events"
  [ jsonRoundtrip
  ]

jsonRoundtrip :: TestTree
jsonRoundtrip = testProperty "Event JSON round-trip" $
  \(ev :: V.Event Int) ->
    Aeson.decode (Aeson.encode ev) == Just ev
