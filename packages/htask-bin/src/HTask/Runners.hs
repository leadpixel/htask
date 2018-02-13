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


runCommand :: Action -> [H.Task] -> IO ()
runCommand List = runList
runCommand (Add tex) = runAdd tex
runCommand (Start ref) = runStart ref
runCommand (Complete ref) = runComplete ref
runCommand (Remove ref) = runRemove ref


runList :: [H.Task] -> IO ()
runList = mapM_ nicePrint


nicePrint :: H.Task -> IO ()
nicePrint t = putStrLn
  (  --show (untag $ H.taskRef t)
  -- <> " | "
  symbolFor t
  <> " "
  <> showDescription t
  )


symbolFor :: H.Task -> String
symbolFor t
  = case H.status t of
      H.Pending    -> "."
      H.InProgress -> ">"
      H.Complete   -> "✓"
      H.Abandoned  -> "-"

showDescription :: H.Task -> String
showDescription t
  =  statusColor (Just $ H.status t)
  <> Text.unpack (H.description t)
  <> statusColor Nothing


statusColor :: Maybe H.TaskStatus -> String
statusColor Nothing             = "\x1b[0m"  -- Clear
statusColor (Just H.Pending)    = "\x1b[34m" -- Blue
statusColor (Just H.InProgress) = "\x1b[33m" -- Yellow
statusColor (Just H.Complete)   = "\x1b[32m" -- Green
statusColor (Just H.Abandoned)  = "\x1b[31m" -- Red


runAdd :: Text.Text -> [H.Task] -> IO ()
runAdd tex ts = do
  k <- runTask (H.addTask tex) ts
  print k


runStart :: Text.Text -> [H.Task] -> IO ()
runStart ref ts
  = maybe
      (print "no")
      (\v -> runTask (H.startTask $ Tagged v) ts >>= print)
      (UUID.fromString $ Text.unpack ref)


runComplete :: Text.Text -> [H.Task] -> IO ()
runComplete ref ts
  = maybe
      (print "no")
      (\v -> runTask (H.completeTask $ Tagged v) ts >>= print)
      (UUID.fromString $ Text.unpack ref)


runRemove :: Text.Text -> [H.Task] -> IO ()
runRemove ref ts
  = maybe
      (print "no")
      (\v -> runTask (H.deleteTask $ Tagged v) ts >>= print)
      (UUID.fromString $ Text.unpack ref)
