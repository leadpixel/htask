{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Tests.TestApp
  ( getEvents
  , getResult
  , getTasks
  , runTestApp
  , runWriteFailure
  ) where

import qualified Control.Monad.Trans.Reader       as Reader
import qualified Control.Monad.Trans.State        as State
import qualified Data.Aeson                       as Aeson
import qualified Data.ByteString.Lazy             as Lazy
import qualified Data.Foldable                    as Foldable
import qualified Data.Map.Strict                  as Map
import qualified HTask.Core                       as H
import qualified Leadpixel.Events                 as V

import           Control.Monad.Trans.Class        (MonadTrans, lift)
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT)
import           Control.Monad.Trans.State        (StateT, runStateT)
import           Data.Map.Strict                  (Map)
import           Data.Sequence                    (Seq, (|>))
import           Data.Time                        (UTCTime)
import           Data.UUID                        (UUID)
import           Leadpixel.Events.Backends.Memory (MemoryBackend,
                                                   runMemoryBackend)
import           Leadpixel.Provider


type Args = (UUID, UTCTime)

newtype TestApp m a
  = TestApp { unTestApp :: StateT (Seq H.Task) (ReaderT Args m) a }
  deriving (Applicative, Functor, H.HasTasks, Monad)

instance (Monad m, V.HasEventSink m) => V.HasEventSink (TestApp m) where
  writeEvent = TestApp . lift . lift . V.writeEvent

instance (Monad m) => Provider UUID (TestApp m) where
  provide = TestApp $ lift ( fst <$> Reader.ask )

instance (Monad m) => Provider UTCTime (TestApp m) where
  provide = TestApp $ lift ( snd <$> Reader.ask )


runTestApp :: (Monad m) => Args -> TestApp (MemoryBackend m) a -> m (a, Map H.TaskUuid H.Task, Seq H.TaskEvent)
runTestApp args op = do
  ((a, b), c) <- runMemoryBackend
      $ flip runReaderT args
      $ runStateT (unTestApp op) mempty

  pure (a, convertToMap b, decodeLog c)

  where
    convertToMap :: Seq H.Task -> Map H.TaskUuid H.Task
    convertToMap = Foldable.foldl' (\b a -> Map.insert (H.taskUuid a) a b) mempty

    decodeLog :: Seq Lazy.ByteString -> Seq H.TaskEvent
    decodeLog = Foldable.foldl' (\b -> maybe b (b |>) . Aeson.decode) mempty


getResult :: (a, b, c) -> a
getResult (a, _, _) = a


getTasks :: (a, b, c) -> b
getTasks (_, b, _) = b


getEvents :: (a, b, c) -> c
getEvents (_, _, c) = c


newtype WriteFailureT m a
  = WriteFailure { unWriteFail :: StateT (Seq H.Task) m a }
  deriving (Applicative, Functor, H.HasTasks, Monad, MonadTrans)


runWriteFailure :: (Monad m) => Args -> WriteFailureT m a -> m (a, Seq H.Task)
runWriteFailure _args op
  = runStateT (unWriteFail op) mempty
