{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tol where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Function
import Data.List
import Data.Monoid
import qualified Data.IntMap.Strict as IM

import Rank

type NodeId = Int

data TolNode = TolNode
    { tRank :: !Rank
    , tName :: !BS.ByteString
    , tKids :: !Tol
    } deriving Show

type Tol = IM.IntMap TolNode

showTol :: Tol -> [BS.ByteString]
showTol !tol = prefixShowTol "" tol

prefixShowTol :: BS.ByteString -> Tol -> [BS.ByteString]
prefixShowTol !prefix !tol =
    concatMap (uncurry (prefixShowIdTolNode prefix)) .
    sortBy (compare `on` tName . snd) $ IM.toList tol

prefixShowIdTolNode :: BS.ByteString -> NodeId -> TolNode -> [BS.ByteString]
prefixShowIdTolNode !prefix !nodeId !(TolNode rank name kids) =
    prefix <> BSC.pack [rankAbbr rank] <> ":" <> name <> " " <>
        BSC.pack (show nodeId) <> "\n" :
    prefixShowTol (prefix <> " ") kids

readTol :: [BS.ByteString] -> Tol
readTol = readTolAccum IM.empty

readTolAccum :: Tol -> [BS.ByteString] -> Tol
readTolAccum !tol [] = tol
readTolAccum !tol (l:ls) = readTolAccum tol' ls2
  where
    (nodeId, tolNode) = readIdTolNode l
    (ls1, ls2) = span (" " `BS.isPrefixOf`) ls
    tol' = IM.insert nodeId (tolNode . readTol $ map BS.tail ls1) tol

readIdTolNode :: BS.ByteString -> (NodeId, Tol -> TolNode)
readIdTolNode s =
    (read $ BSC.unpack rest2, TolNode (abbrToRank rank) (BS.init rest1Sp))
  where
    Just (rank, s1) = BSC.uncons s
    Just (':', rest) = BSC.uncons s1
    (rest1Sp, rest2) = BSC.breakEnd (== ' ') rest

readTolF :: FilePath -> IO Tol
readTolF tolF = do
    tol <- readTol . BSC.lines <$> BS.readFile tolF
    --let (fol', dupes) = folFindDupes fol
    --mapM_ print dupes
    --return fol'
    return tol

writeTolF :: FilePath -> Tol -> IO ()
writeTolF tolF = BSL.writeFile tolF . BSL.fromChunks . showTol
