{-# LANGUAGE OverloadedStrings #-}

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


type SomeEvent = Event Aeson.Value


main :: IO ()
main
  = (sortEvents <$> readEvents file)
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


sortEvents :: [Maybe (Event a)] -> [Event a]
sortEvents = sortOn timestamp . catMaybes
