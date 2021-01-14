{-# LANGUAGE OverloadedStrings #-}

module APITests.Complete
  ( testComplete
  ) where

import qualified Data.UUID                 as UUID
import qualified HTask.Core.API            as API
import qualified HTask.Core.Task           as H

import           APITestMonad              (runApi)
import           Data.Tagged               (Tagged (..))
import           Data.Time                 (Day (ModifiedJulianDay),
                                            UTCTime (..))
import           Test.QuickCheck.Instances ()
import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.HUnit          (assertEqual, testCase)


fakeTime :: UTCTime
fakeTime = UTCTime (ModifiedJulianDay 0) 0


testComplete :: TestTree
testComplete = testGroup "complete"
  [ testSuccess
  , testFailure
  ]

  where
    testSuccess = testGroup "success"
      [ returnsCreatedUuid
      ]

    testFailure = testGroup "failure"
      [ failsWhenUnableToFindMatch
      ]


returnsCreatedUuid :: TestTree
returnsCreatedUuid = testCase "returns the created uuid on success" $ do
  uuid <- F.uuidGen
  x <- runApi (uuid, fakeTime) (API.addTask "some task" >> API.completeTask (UUID.toText uuid))
  assertEqual "" (f uuid) x
    where
      f uuid = API.ModifySuccess
        ( H.Task
          { H.taskRef = Tagged uuid
          , H.description = "some task"
          , H.createdAt = fakeTime
          , H.status = H.Complete
          }
        )


failsWhenUnableToFindMatch :: TestTree
failsWhenUnableToFindMatch = testCase "fails when unable to find a matching task" $ do
  uuid <- F.uuidGen
  x <- runApi (uuid, fakeTime) (API.addTask "some task" >> API.completeTask "unknown")
  assertEqual "expecting failed to find" API.FailedToFind x
