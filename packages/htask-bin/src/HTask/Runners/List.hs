module HTask.Runners.List
  ( runList
  ) where

import Data.List
import Data.Semigroup ((<>))
import Data.Tagged
import HTask.Actions
import HTask.TaskApplication
import qualified Data.Text              as Text
import qualified Data.UUID as UUID
import qualified HTask as H


taskDisplayOrder :: H.Task -> H.Task -> Ordering
taskDisplayOrder a b = statusDisplayOrder (H.status a) (H.status b)


statusDisplayOrder :: H.TaskStatus -> H.TaskStatus -> Ordering
statusDisplayOrder   H.InProgress  H.InProgress   =   EQ
statusDisplayOrder   H.Pending     H.InProgress   =   GT
statusDisplayOrder   H.Complete    H.InProgress   =   GT
statusDisplayOrder   H.Abandoned   H.InProgress   =   GT

statusDisplayOrder   H.InProgress  H.Pending      =   LT
statusDisplayOrder   H.Pending     H.Pending      =   EQ
statusDisplayOrder   H.Complete    H.Pending      =   GT
statusDisplayOrder   H.Abandoned   H.Pending      =   GT

statusDisplayOrder   H.InProgress  H.Complete     =   LT
statusDisplayOrder   H.Pending     H.Complete     =   LT
statusDisplayOrder   H.Complete    H.Complete     =   EQ
statusDisplayOrder   H.Abandoned   H.Complete     =   GT

statusDisplayOrder   H.InProgress  H.Abandoned    =   LT
statusDisplayOrder   H.Pending     H.Abandoned    =   LT
statusDisplayOrder   H.Complete    H.Abandoned    =   LT
statusDisplayOrder   H.Abandoned   H.Abandoned    =   EQ


runList :: DetailFlag -> FilePath -> IO ()
runList d file
  = do
    ts <- runTask H.listTasks file
    mapM_
      (mapM_ (nicePrint d))
      (groupBy sameStatus $ sortBy taskDisplayOrder ts)

  where
    sameStatus :: H.Task -> H.Task -> Bool
    sameStatus a b = H.status a == H.status b


nicePrint :: DetailFlag -> H.Task -> IO ()
nicePrint (HideDetail) t = putStrLn
  (  symbolFor t
  <> " "
  <> showDescription t
  )

nicePrint (ShowDetail) t = putStrLn
  (  show (untag $ H.taskRef t)
  <> " "
  <> symbolFor t
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

