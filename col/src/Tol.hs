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
import qualified Data.Map.Strict as M

import Rank

type TaxonId = BS.ByteString

data INode a = INode
    { iVal :: !a
    , iKids :: !(ITree a)
    }

type ITree a = M.Map TaxonId (INode a)

data Taxon = Taxon
    { tRank :: !Rank
    , tName :: !BS.ByteString
    } deriving (Eq, Ord, Show)

type Tol = ITree Taxon

doCollapseSuperfamilies :: Tol -> Tol
doCollapseSuperfamilies = kidMitosis myF
  where
    myF :: TaxonId -> INode Taxon -> Tol
    myF _ (INode (Taxon Superfamily _) kids) = kids
    myF i (INode a kids) = M.singleton i (INode a $ kidMitosis myF kids)

kidMitosis :: (TaxonId -> INode a -> ITree a) -> ITree a -> ITree a
kidMitosis f = M.unions . map (uncurry f) . M.toList

filterITree :: (a -> Bool) -> ITree a -> ITree a
filterITree f =
    M.map (\(INode a kids) -> INode a $ filterITree f kids) .
    M.filter (f . iVal)

flattenITree :: ITree a -> [a]
flattenITree = concatMap (\(INode a kids) -> a : flattenITree kids) . M.elems

showTol :: Bool -> Tol -> [BS.ByteString]
showTol showId = showITree (showIdTaxon showId) (compare `on` tName)

showIdTaxon :: Bool -> TaxonId -> Taxon -> BS.ByteString
showIdTaxon showId !taxonId !taxon = showTaxon taxon <>
    (if showId then " " <> taxonId else "")

showTaxon :: Taxon -> BS.ByteString
showTaxon !(Taxon rank name) = BSC.pack [rankAbbr rank] <> ":" <> name

showITree
    :: (TaxonId -> a -> BS.ByteString)
    -> (a -> a -> Ordering)
    -> ITree a
    -> [BS.ByteString]
showITree = prefixShowITree ""

-- todo: This isn't Taxon agnostic..
prefixShowITree
    :: BS.ByteString
    -> (TaxonId -> a -> BS.ByteString)
    -> (a -> a -> Ordering)
    -> ITree a
    -> [BS.ByteString]
prefixShowITree !prefix f cmp !tol =
    concatMap (\(i, INode a kids) ->
        prefix <> f i a : prefixShowITree (prefix <> " ") f cmp kids) .
    sortBy (cmp `on` iVal . snd) $ M.toList tol

mapITree :: (a -> b) -> ITree a -> ITree b
mapITree f = M.map (\(INode a kids) -> INode (f a) $ mapITree f kids)

readTol :: [BS.ByteString] -> Tol
readTol = readTolAccum M.empty

readTolAccum :: Tol -> [BS.ByteString] -> Tol
readTolAccum !tol [] = tol
readTolAccum !tol (l:ls) = readTolAccum tol' ls2
  where
    (taxonId, taxon) = readIdTaxon l
    (ls1, ls2) = span (" " `BS.isPrefixOf`) ls
    tol' = M.insert taxonId (INode taxon . readTol $ map BS.tail ls1) tol

readIdTaxon :: BS.ByteString -> (TaxonId, Taxon)
readIdTaxon s =
    (taxonId, Taxon (abbrToRank rank) (BS.init rest1Sp))
  where
    Just (rank, s1) = BSC.uncons s
    Just (':', rest) = BSC.uncons s1
    (rest1Sp, taxonId) = BSC.breakEnd (== ' ') rest

readTolF :: FilePath -> IO Tol
readTolF tolF = do
    tol <- readTol . BSC.lines <$> BS.readFile tolF
    --let (fol', dupes) = folFindDupes fol
    --mapM_ print dupes
    --return fol'
    return tol

writeTolF :: Bool -> FilePath -> Tol -> IO ()
writeTolF showId tolF =
    BSL.writeFile tolF . BSL.fromChunks . map (<> "\n") . showTol showId
