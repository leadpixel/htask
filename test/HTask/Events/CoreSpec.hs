{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HTask.Events.CoreSpec (tests) where

import qualified Data.Aeson                     as Aeson
import qualified HTask.Events                   as Events

import           Test.QuickCheck
import           Test.QuickCheck.Instances.Time ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

instance (Arbitrary a) => Arbitrary (Events.Event a) where
  arbitrary = Events.Event <$> arbitrary <*> arbitrary

tests :: TestTree
tests = testGroup "leadpixel-events"
  [ jsonRoundtrip
  ]

jsonRoundtrip :: TestTree
jsonRoundtrip = testProperty "Event JSON round-trip" $
  \(ev :: Events.Event Int) ->
    Aeson.decode (Aeson.encode ev) == Just ev
