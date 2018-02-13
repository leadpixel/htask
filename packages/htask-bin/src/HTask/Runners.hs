module HTask.Runners
  ( runCommand
  ) where

import qualified HTask as H
import HTask.Actions
import Data.Semigroup ((<>))
import Data.Tagged
import HTask.TaskApplication
import qualified Data.Text              as Text
import qualified Data.UUID as UUID


runCommand :: [H.Task] -> Action -> IO ()
runCommand ts List = mapM_ nicePrint ts
runCommand ts (Add tex) = runAdd ts tex
runCommand ts (Start ref) = runStart ts ref
runCommand ts (Remove ref) = runRemove ts ref


runAdd :: [H.Task] -> Text.Text -> IO ()
runAdd ts tex = do
  k <- runTask ts (H.addTask tex)
  print k


runStart :: [H.Task] -> Text.Text -> IO ()
runStart ts ref
  = maybe
      (print "no")
      (\v -> runTask ts (H.startTask $ Tagged v) >>= print)
      (UUID.fromString $ Text.unpack ref)


runRemove :: [H.Task] -> Text.Text -> IO ()
runRemove ts ref
  = maybe
      (print "no")
      (\v -> runTask ts (H.deleteTask $ Tagged v) >>= print)
      (UUID.fromString $ Text.unpack ref)



nicePrint :: H.Task -> IO ()
nicePrint t = putStrLn
  (  show (untag $ H.taskRef t)
  <> " | "
  <> (Text.unpack $ H.description t)
  )
