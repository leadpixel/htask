{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Tests.TestApp
  ( TestApp
  , getResult
  , getTasks
  , runTestApp
  ) where

import qualified Control.Monad.Trans.Reader       as Reader
import qualified Control.Monad.Trans.State        as State
import qualified Control.Monad.Trans.Writer       as Writer
import qualified Data.Sequence                    as Seq
import qualified Data.UUID.V4                     as UUID
import qualified HTask.Core                       as H
import qualified Leadpixel.Events                 as V

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader)
import           Control.Monad.State              (MonadState)
import           Control.Monad.Trans.Class        (MonadTrans, lift)
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT)
import           Control.Monad.Trans.State        (StateT, runStateT)
import           Control.Monad.Trans.Writer       (WriterT, runWriterT)
import           Control.Monad.Writer             (MonadWriter)
import           Data.Sequence                    (Seq)
import           Data.Time                        (UTCTime)
import           Data.UUID                        (UUID)
import           Leadpixel.Events.Backends.Memory (MemoryBackend,
                                                   runMemoryBackend)
import           Leadpixel.Provider


type Args = UTCTime

newtype TestApp m a
  = TestApp { unTestApp :: StateT H.TaskMap (WriterT (Seq UUID) (ReaderT Args (MemoryBackend m))) a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadState H.TaskMap, MonadFail)

instance (Monad m) => V.HasEventSink (TestApp m) where
  writeEvent = TestApp . lift . lift . lift . V.writeEvent

instance (MonadIO m) => Provider UUID (TestApp m) where
  provide = do
    uuid <- liftIO UUID.nextRandom
    TestApp $ lift $ Writer.tell (Seq.singleton uuid)
    pure uuid


instance (Monad m) => Provider UTCTime (TestApp m) where
  provide = TestApp $ lift $ lift Reader.ask


data TestOutput a
  = TestOutput
    { testResult :: a
    , tasks      :: H.TaskMap
    , uuids      :: Seq UUID
    }
  deriving (Show)


runTestApp :: (Monad m) => Args -> TestApp m a -> m (TestOutput a)
runTestApp args op = do
  (((a, b) , xs), _evs) <- runMemoryBackend
      $ flip runReaderT args
      $ runWriterT
      $ runStateT (unTestApp op) mempty

  pure $ TestOutput
    { testResult = a
    , tasks = b
    , uuids = xs
    }


getResult :: TestOutput a -> a
getResult = testResult


getTasks :: TestOutput a -> H.TaskMap
getTasks = tasks
