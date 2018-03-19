{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString              as S
import qualified Data.ByteString.Lazy         as L
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Combinators     as Cx
import qualified Data.List                    as List
import qualified Data.Maybe                   as M

import Event
import Data.Semigroup ((<>))
import Data.Conduit (($$), ($=))


type SomeEvent = Event Aeson.Value


main :: IO ()
main
  = (sortEvents <$> conduitReadEvents file)
  >>= conduitWriteEvents file

  where
    file = ".tasks"


conduitReadEvents :: FilePath -> IO [Maybe SomeEvent]
conduitReadEvents file = do
  content <- R.runResourceT
    $ Cx.sourceFile file
    $= splitLines
    $$ Cx.sinkList

  pure (Aeson.decodeStrict <$> content)


conduitWriteEvents :: FilePath -> [SomeEvent] -> IO ()
conduitWriteEvents file xs
  = R.runResourceT
    $ Cx.yieldMany (convert <$> xs)
    $$ Cx.sinkFile file

  where
    convert :: SomeEvent -> S.ByteString
    convert e = L.toStrict (Aeson.encode e) <> "\n"


splitLines :: C.Conduit S.ByteString (R.ResourceT IO) S.ByteString
splitLines = Cx.linesUnboundedAscii


sortEvents :: [Maybe (Event a)] -> [Event a]
sortEvents = List.sortOn timestamp . M.catMaybes
