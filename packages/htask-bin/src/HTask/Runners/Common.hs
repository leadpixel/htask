{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Common
  ( runWithMatch
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.TaskApplication
import HTask.Output
import qualified Data.UUID as UUID
import Data.Semigroup ((<>))
import Data.Tagged


justOne :: [a] -> Maybe a
justOne [x] = Just x
justOne _ = Nothing


findMatchingUUIDs :: Text.Text -> [H.Task] -> [Text.Text]
findMatchingUUIDs ref ts = filter (ref `Text.isPrefixOf`) (fmap taskRefToText ts)


taskRefToText :: H.Task -> Text.Text
taskRefToText = UUID.toText . untag . H.taskRef


runWithMatch :: (Show a) => (H.TaskRef -> TaskApplication a) -> Text.Text -> TaskConfig Document
runWithMatch f ref
  = do
    ts <- runTask H.listTasks
    output <- maybe
      (pure $ "did not find unique match for: " <> ref)
      (\v -> (Text.pack . show) <$> runTask (f $ Tagged v))
      (justOne (findMatchingUUIDs ref ts) >>= UUID.fromText)
    pure $ Document [ line output ]
