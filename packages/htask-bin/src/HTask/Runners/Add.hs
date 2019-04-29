{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Add
  ( runAdd
  ) where

import qualified Data.Text             as Text
import qualified HTask                 as H

import           Data.Text             (Text)
import           HTask.Output
import           HTask.TaskApplication

import           Data.Semigroup        ((<>))


type AddOutput = Either String H.TaskRef


runAdd :: (HasEventBackend m, H.CanCreateTask m) => Text -> m RunResult
runAdd t
  = presentAdd t <$> runTask (H.addTask t)


presentAdd :: Text -> AddOutput -> RunResult
presentAdd t
  = either
      (resultError . Text.pack)
      resultSuccessAdd

  where
    resultSuccessAdd ref
      = resultSuccess
          [ "added task: " <> t
          , "ref: " <> H.taskRefText ref
          ]
