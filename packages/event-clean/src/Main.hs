{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Trans.Resource as Rt
import qualified Data.Aeson                   as A
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Combinators     as Cx
import qualified Data.List                    as List
import qualified Data.Maybe                   as M

import Event
import Data.Semigroup ((<>))
import Data.Conduit (($$), ($=))


type SomeEvent = Event A.Value


main :: IO ()
main
  = (sortEvents <$> conduitReadEvents file)
  >>= conduitWriteEvents file

  where
    file = ".tasks"


conduitReadEvents :: FilePath -> IO [Maybe SomeEvent]
conduitReadEvents file = do
  content <- Rt.runResourceT
    $ Cx.sourceFile file
    $= splitLines
    $$ Cx.sinkList

  pure (A.decodeStrict <$> content)


conduitWriteEvents :: FilePath -> [SomeEvent] -> IO ()
conduitWriteEvents file xs
  = Rt.runResourceT
    $ Cx.yieldMany (convert <$> xs)
    $$ Cx.sinkFile file

  where
    convert :: SomeEvent -> BS.ByteString
    convert e = BL.toStrict (A.encode e) <> "\n"


splitLines :: C.Conduit BS.ByteString (Rt.ResourceT IO) BS.ByteString
splitLines = Cx.linesUnboundedAscii


sortEvents :: [Maybe (Event a)] -> [Event a]
sortEvents = List.sortOn timestamp . M.catMaybes
