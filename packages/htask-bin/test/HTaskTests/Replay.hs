module HTaskTests.Replay
  where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import qualified Control.Monad.State as State
import qualified Control.Monad.Writer as Writer
import qualified HTask as H


runTaskApi :: H.TaskMonad H.Tasks -> IO (H.Tasks, H.EventLog)
runTaskApi op
  = State.evalStateT
      (Writer.runWriterT op)
      H.emptyTasks


test_rebuild :: TestTree
test_rebuild = testProperty "can rebuild task state from log" thing
  where
    thing :: Property
    thing = forAll genTaskInteractions $ \p ->
      monadicIO $ run $ do
        (a, ls) <- runTaskApi (p >> H.listTasks)
        let b = rebuildFromLog ls
        assertEqual "rebuild should be identical" a b


genTaskInteractions :: Gen (H.TaskMonad H.Tasks)
genTaskInteractions = undefined


rebuildFromLog :: H.EventLog -> H.Tasks
rebuildFromLog = undefined

