{-# LANGUAGE OverloadedStrings #-}

module HTask.Runners.Common
  ( runWithMatch
  , findMatch
  ) where

import qualified Data.Text as Text
import qualified HTask as H
import HTask.TaskApplication
import HTask.Output
import qualified Data.UUID as UUID
import Data.Semigroup ((<>))
import Data.Tagged


headSafe :: [a] -> Maybe a
headSafe [x] = Just x
headSafe _ = Nothing


taskRefToText :: H.Task -> Text.Text
taskRefToText = UUID.toText . untag . H.taskRef


findMatch :: Text.Text -> TaskConfig (Maybe H.Task)
findMatch ref
  = (headSafe . filterMatchesUUID ref) <$> runTask H.listTasks

 where
   filterMatchesUUID :: Text.Text -> [H.Task] -> [H.Task]
   filterMatchesUUID t
     = filter (Text.isPrefixOf t . taskRefToText)


runWithMatch
  :: (Show a)
  => (H.TaskRef -> TaskApplication a)
  -> Text.Text
  -> TaskConfig Document
runWithMatch f ref
  = findMatch ref
  >>= maybe
      (pure $ formatNoMatchError ref)
      ((fmap . fmap) coerceToSuccess (j f))

  where
    formatNoMatchError :: Text.Text -> Document
    formatNoMatchError t
      = formatError ("did not find unique match for " <> t)

    j :: (Show a) => (H.TaskRef -> TaskApplication a) -> H.Task -> TaskConfig a
    j f' v
      = runTask (f' $ H.taskRef v)

    coerceToSuccess :: (Show a) => a -> Document
    coerceToSuccess = formatSuccess . Text.pack . show
