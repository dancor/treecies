{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TolWork where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid

import Disp
import Rank
import Tol
import Web

type Counts = M.Map Rank Int

type CountTol = ITree (Taxon, Counts)

tolCalcCounts :: Tol -> CountTol
tolCalcCounts = M.map tolNodeCalcCounts

tolNodeCalcCounts :: INode Taxon -> INode (Taxon, Counts)
tolNodeCalcCounts (INode t@(Taxon rank _) kids) =
    INode (t, counts) kidCounts
  where
    kidCounts = tolCalcCounts kids
    counts = M.unionsWith (+) $
        M.singleton rank 1 : map (snd . iVal) (M.elems kidCounts)

tolSummary :: Tol -> [BS.ByteString]
tolSummary tol = concat
    [ renderCols (
      ["", "P", "C", "O", "F", "G", "S"] :
      showNodeCounts "" lev0Counts :
      replicate 7 "" :
      map (showNodeCounts "- ") lev1Counts)
    , [""]
    , renderCols $ intercalate (replicate 2 $ replicate 5 "")
      [ intercalate [replicate 5 ""] $ catMaybes
        [ if name /= "Viruses" || rank >= Order
          then Just $ rankWithMost name rank innerRank kids
          else Nothing
        | innerRank <- mainRanks
        , innerRank > rank
        ]
      | INode (Taxon _ name, _) kids <- lev1Counts
      , rank <- init $ tail mainRanks
      ]
    ]
  where
    lev0Counts = tolNodeCalcCounts $ INode (Taxon RankAll "All Life") tol
    lev1Counts = sortBy (flip compare `on` M.lookup Genus . snd . iVal) .
        M.elems $ iKids lev0Counts

showNodeCounts :: BS.ByteString -> INode (Taxon, Counts) -> [BS.ByteString]
showNodeCounts pre (INode (Taxon _ name, counts) _) =
    (pre <> name <> ":") :
    map (BSC.pack . show . snd) (procCounts Kingdom counts)

procCounts :: Rank -> Counts -> [(Rank, Int)]
procCounts rank = killSomeRanks . killFirstIfOne . M.toList
  where
    killFirstIfOne ((_, 1) : rest) = rest
    killFirstIfOne a = a
    killSomeRanks = filter ((\r -> r /= Superfamily && r > rank) . fst)

rankWithMost
    :: BS.ByteString
    -> Rank
    -> Rank
    -> ITree (Taxon, Counts)
    -> [[BS.ByteString]]
rankWithMost name rank innerRank tolCounts = zipWith (++)
    (label : repeat labelSp)
    (map (:[]) results)
  where
    label = [name, rankPlural rank, "with most", rankPlural innerRank <> ":"]
    labelSp = replicate 4 ""
    results = take 5 .
          map (\(name2, c) -> name2 <> BSC.pack (' ' : show c)) .
          sortBy (flip compare `on` snd) .
          map (second $ M.findWithDefault 0 innerRank) .
          map (first tName) .
          filter ((== rank) . tRank . fst) $
          flattenITree tolCounts

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

mapAccumWithKeyM
    :: (Eq k, Functor m, Monad m)
    => (acc -> k -> x -> m (acc, y))
    -> acc
    -> M.Map k x
    -> m (acc, M.Map k y)
mapAccumWithKeyM f acc =
    fmap (second M.fromAscList) .
    mapAccumM (\acc2 (k, v) -> second ((,) k) <$> f acc2 k v) acc .
    M.toAscList

growTol :: Rank -> Int -> Tol -> IO (Int, Tol)
growTol !maxRank !growsLeft !tol =
    mapAccumWithKeyM (growTolAccum maxRank) growsLeft tol

onIVal :: (a -> a) -> INode a -> INode a
onIVal f (INode a kids) = INode (f a) kids

growTolAccum :: Rank -> Int -> TaxonId -> INode Taxon -> IO (Int, INode Taxon)
growTolAccum !maxRank !growsLeft !nodeId
        !iNode@(INode (Taxon rank name) kids) = do
    if growsLeft <= 0 || rank >= maxRank
      then return (growsLeft, iNode)
      else if M.null kids
        then do
          BSC.putStrLn $ BSC.pack (show rank) <> " " <> name <> " " <>
              BSC.pack (show nodeId)
          kids2 <- idGetTol nodeId
          let kids3 = if rank == Genus
                then M.map
                  (onIVal $ \tN ->
                      tN {tName = speciesKillGenus name (tName tN)})
                  kids2
                else kids2
          return (growsLeft - 1, INode (Taxon rank name) kids3)
        else do
          (growsLeft', kids') <- growTol maxRank growsLeft kids
          return (growsLeft', INode (Taxon rank name) kids')

speciesKillGenus :: BS.ByteString -> BS.ByteString -> BS.ByteString
-- Some species have no names, weirdly.
speciesKillGenus _genus "" = ""
speciesKillGenus genus species =
    if genusSp `BS.isPrefixOf` species
      then BS.drop (BS.length genusSp) species
      -- else error "Species name didn't have genus as prefix."
      else species <> "-(Species-name-did-not-have-genus-as-prefix.)"
  where
    genusSp = genus <> " "
