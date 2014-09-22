{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TolWork where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.IntMap as IM
import Data.Monoid

import Rank
import Tol
import Web

{-
treeKidCount :: Tree a -> Tree (a, Int)
treeKidCount (Node n kids) = Node (n, length kids) (forestKidCount kids)

forestKidCount :: Forest a -> Forest (a, Int)
forestKidCount = map treeKidCount

folCalcCounts :: Fol -> Forest (TolNode, M.Map Rank Int)
folCalcCounts = map tolCalcCounts

tolCalcCounts :: Tol -> Tree (TolNode, M.Map Rank Int)
tolCalcCounts (Node tn kids) =
    Node (tn, counts) kids'
  where
    kids' = folCalcCounts kids
    kidsCounts = map (snd . rootLabel) kids'
    counts =
        M.unionsWith (+)
        ((M.singleton (tnRank tn) 1) : kidsCounts)

showCounts :: M.Map Rank Int -> BS.ByteString
showCounts =
    BS.intercalate (BSC.pack "\t") .
    map (\(rank, i) -> BSC.pack $ rankAbbr rank : ':' : show i) .
    killSomeRanks .
    killFirstIfOne .
    M.toList
  where
    killFirstIfOne ((_, 1) : rest) = rest
    killFirstIfOne a = a
    killSomeRanks = filter ((`notElem` [Kingdom, Superfamily]) . fst)

showTolCounts :: Tree (TolNode, M.Map Rank Int) -> BS.ByteString
showTolCounts (Node (tn, c) _) =
    BS.concat [tnName tn, ":\t", showCounts c]

showFolCounts :: Forest (TolNode, M.Map Rank Int) -> [BS.ByteString]
showFolCounts = map (("- " `BS.append`) . showTolCounts)

travelDown
    :: BS.ByteString
    -> Forest (TolNode, M.Map Rank Int)
    -> Forest (TolNode, M.Map Rank Int)
travelDown s = subForest . head . filter ((== s) . tnName . fst . rootLabel)

rankWithMost
     :: Rank
     -> Rank
     -> [Tree (TolNode, M.Map Rank Int)]
     -> TolNode
     -> BSC.ByteString
rankWithMost rank innerRank kids tolNode =
    BSC.unlines $
        zipWith BS.append
            (label : repeat labelSp)
            results
  where
    label = BS.concat
        [ tnName tolNode, "   \t", rankPlural rank, " with most\t"
        , rankPlural innerRank, ":  \t"
        ]
    labelSp = BSC.map (\ c -> if isSpace c then c else ' ') label
    results = take 5 .
          map (\ (tn, c) ->
              BS.concat [tnName tn, BSC.pack (' ' : show c)]) .
          sortBy (flip $ comparing snd) .
          map (second $ M.findWithDefault 0 innerRank) .
          -- filter ((/= "Not assigned") . tnName . fst) $
          filter ((== rank) . tnRank . fst) $
          concatMap flatten kids

folSummary :: Fol -> [BS.ByteString]
folSummary fol = concat
    [ [showTolCounts topCounts]
    , showFolCounts folCounts
    , [""]
    , filter (not . BS.null) [ BSC.unlines $ catMaybes
        [ if tnName tn /= "Viruses" || rank >= Family
          then Just $ rankWithMost rank innerRank kids tn
          else Nothing
        | innerRank <- mainRanks
        , innerRank > rank
        ]
      | Node (tn, _) kids <- folCounts
      , rank <- init $ tail mainRanks
      ]
    ]
  where
    topCounts = tolCalcCounts $ Node (TolNode 0 "All Life" RankAll) fol
    folCounts = subForest topCounts
-}

{-
folFindDupes :: Fol -> (Fol, [[TolNode]])
folFindDupes fol =
    second ((dupes ++) . concat) . unzip .
    map tolFindDupes $ nubBy ((==) `on` (tnName . rootLabel)) fol
  where
    dupes =
        filter ((> 1) . length) .
        groupBy ((==) `on` tnName) $
        sortBy (comparing tnName) $
        map rootLabel fol

tolFindDupes :: Tol -> (Tol, [[TolNode]])
tolFindDupes (Node tn kids) = first (Node tn) $ folFindDupes kids
-}

mapAccumM
    :: Monad m
    => (acc -> x -> m (acc, y))
    -> acc
    -> [x]
    -> m (acc, [y])
mapAccumM _ acc [] = return (acc, [])
mapAccumM f acc (x:xs) = do
    (acc2, y) <- f acc x
    (acc3, ys) <- mapAccumM f acc2 xs
    return (acc3, y:ys)

intMapAccumWithKeyM
    :: (Functor m, Monad m)
    => (acc -> Int -> x -> m (acc, y))
    -> acc
    -> IM.IntMap x
    -> m (acc, IM.IntMap y)
intMapAccumWithKeyM f acc =
    fmap (second IM.fromAscList) .
    mapAccumM (\acc2 (k, v) -> second ((,) k) <$> f acc2 k v) acc .
    IM.toAscList

growTol :: Rank -> Int -> Tol -> IO (Int, Tol)
growTol !maxRank !growsLeft !tol =
    intMapAccumWithKeyM (growTolAccum maxRank) growsLeft tol

growTolAccum :: Rank -> Int -> NodeId -> TolNode -> IO (Int, TolNode)
growTolAccum !maxRank !growsLeft !nodeId !tolNode@(TolNode rank name kids) = do
    if growsLeft <= 0 || rank >= maxRank
      then return (growsLeft, tolNode)
      else if IM.null kids
        then do
          BSC.putStrLn $ BSC.pack (show rank) <> " " <> name <> " " <>
              BSC.pack (show nodeId)
          kids2 <- idGetTol nodeId
          let kids3 = if rank == Genus
                then IM.map
                  (\tN -> tN {tName = speciesKillGenus name (tName tN)})
                  kids2
                else kids2
          return (growsLeft - 1, TolNode rank name kids3)
        else do
          (growsLeft', kids') <- growTol maxRank growsLeft kids
          return (growsLeft', TolNode rank name kids')

speciesKillGenus :: BS.ByteString -> BS.ByteString -> BS.ByteString
-- Some species have no names, weirdly.
speciesKillGenus _genus "" = ""
speciesKillGenus genus species =
    if genusSp `BS.isPrefixOf` species
      then BS.drop (BS.length genusSp) species
      else error "Species name didn't have genus as prefix."
  where
    genusSp = genus <> " "
