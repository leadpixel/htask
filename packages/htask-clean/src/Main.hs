{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad.Trans.Resource
import Data.Conduit as C
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.Combinators as Cx
import qualified HTask as H


type SomeEvent = H.Event K


data K = K Aeson.Value
  deriving (Show)

instance Aeson.ToJSON K where
  toJSON (K v) = v

instance Aeson.FromJSON K where
  parseJSON x = pure (K x)


main :: IO ()
main
  = readEvents file
  >>= pure . sortEvents
  >>= writeEvents file

  where
    file = ".tasks"


readEvents :: FilePath -> IO [Maybe SomeEvent]
readEvents file = do
  content <- runResourceT
    $ Cx.sourceFile file
    $= splitLines
    $$ Cx.sinkList

  pure (Aeson.decodeStrict <$> content)


writeEvents :: FilePath -> [SomeEvent] -> IO ()
writeEvents file xs
  = runResourceT
    $ Cx.yieldMany (convert <$> xs)
    $$ Cx.sinkFile file

  where
    convert :: SomeEvent -> S.ByteString
    convert e = L.toStrict (Aeson.encode e) <> "\n"


splitLines :: Conduit S.ByteString (ResourceT IO) S.ByteString
splitLines = Cx.linesUnboundedAscii


sortEvents :: [Maybe (H.Event a)] -> [H.Event a]
sortEvents = sortOn H.timestamp . catMaybes
