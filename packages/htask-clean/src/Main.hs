{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main
  ( main
  ) where

import Control.Monad.Trans.Resource
import Data.Conduit as C
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))
import Event
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.Combinators as Cx


newtype K f m a = K (f -> (m a))


instance EventBackend (K FilePath IO) where
  readEvents = K kkk
  writeEvent = K . flip jjj


kkk :: (Aeson.FromJSON a) => FilePath -> IO [Event a]
kkk file
  = t <$> runResourceT
      (  Cx.sourceFile file
      $= splitLines
      $$ Cx.sinkList
      )

  where
    t :: (Aeson.FromJSON a) => [S.ByteString] -> [Event a]
    t = catMaybes . fmap Aeson.decodeStrict


jjj :: (Aeson.ToJSON a) => FilePath -> Event a -> IO ()
jjj file xs
  = runResourceT
    $ C.yield (convert xs)
    $$ Cx.sinkFile file

  where
    convert :: (Aeson.ToJSON a) => Event a -> S.ByteString
    convert e = L.toStrict (Aeson.encode e) <> "\n"






type SomeEvent = Event Aeson.Value


main :: IO ()
main
  = (sortEvents <$> conduitReadEvents file)
  >>= conduitWriteEvents file

  where
    file = ".tjasks"


conduitReadEvents :: FilePath -> IO [Maybe SomeEvent]
conduitReadEvents file = do
  content <- runResourceT
    $ Cx.sourceFile file
    $= splitLines
    $$ Cx.sinkList

  pure (Aeson.decodeStrict <$> content)


conduitWriteEvents :: FilePath -> [SomeEvent] -> IO ()
conduitWriteEvents file xs
  = runResourceT
    $ Cx.yieldMany (convert <$> xs)
    $$ Cx.sinkFile file

  where
    convert :: SomeEvent -> S.ByteString
    convert e = L.toStrict (Aeson.encode e) <> "\n"


splitLines :: Conduit S.ByteString (ResourceT IO) S.ByteString
splitLines = Cx.linesUnboundedAscii


sortEvents :: [Maybe (Event a)] -> [Event a]
sortEvents = sortOn timestamp . catMaybes
