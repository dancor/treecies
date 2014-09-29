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

data INode a = INode
    { iVal :: !a
    , iKids :: !(ITree a)
    }

type ITree a = IM.IntMap (INode a)

data Taxon = Taxon
    { tRank :: !Rank
    , tName :: !BS.ByteString
    } deriving Show

type Tol = ITree Taxon

filterITree :: (a -> Bool) -> ITree a -> ITree a
filterITree f =
    IM.map (\(INode a kids) -> INode a $ filterITree f kids) .
    IM.filter (f . iVal)

flattenITree :: ITree a -> [a]
flattenITree = concatMap (\(INode a kids) -> a : flattenITree kids) . IM.elems

showTol :: Tol -> [BS.ByteString]
showTol = showITree showIdTaxon (compare `on` tName)

showIdTaxon :: Int -> Taxon -> BS.ByteString
showIdTaxon !taxonId !(Taxon rank name) = BSC.pack [rankAbbr rank] <> ":" <>
    name <> " " <> BSC.pack (show taxonId)

showITree
    :: (Int -> a -> BS.ByteString)
    -> (a -> a -> Ordering)
    -> ITree a
    -> [BS.ByteString]
showITree = prefixShowITree ""

-- todo: This isn't Taxon agnostic..
prefixShowITree
    :: BS.ByteString
    -> (Int -> a -> BS.ByteString)
    -> (a -> a -> Ordering)
    -> ITree a
    -> [BS.ByteString]
prefixShowITree !prefix f cmp !tol =
    concatMap (\(i, INode a kids) ->
        prefix <> f i a : prefixShowITree (prefix <> " ") f cmp kids) .
    sortBy (cmp `on` iVal . snd) $ IM.toList tol

mapITree :: (a -> b) -> ITree a -> ITree b
mapITree f = IM.map (\(INode a kids) -> INode (f a) $ mapITree f kids)

readTol :: [BS.ByteString] -> Tol
readTol = readTolAccum IM.empty

readTolAccum :: Tol -> [BS.ByteString] -> Tol
readTolAccum !tol [] = tol
readTolAccum !tol (l:ls) = readTolAccum tol' ls2
  where
    (taxonId, taxon) = readIdTaxon l
    (ls1, ls2) = span (" " `BS.isPrefixOf`) ls
    tol' = IM.insert taxonId (INode taxon . readTol $ map BS.tail ls1) tol

readIdTaxon :: BS.ByteString -> (Int, Taxon)
readIdTaxon s =
    (read $ BSC.unpack rest2, Taxon (abbrToRank rank) (BS.init rest1Sp))
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
writeTolF tolF = BSL.writeFile tolF . BSL.fromChunks . map (<> "\n") . showTol
