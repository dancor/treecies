{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TolWork where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import qualified Data.IntMap.Strict as IM
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid

import Rank
import Tol
import Web

type Counts = M.Map Rank Int

tolCalcCounts :: ITree Taxon -> ITree (Taxon, Counts)
tolCalcCounts = IM.map tolNodeCalcCounts

tolNodeCalcCounts :: INode Taxon -> INode (Taxon, Counts)
tolNodeCalcCounts (INode t@(Taxon rank _) kids) =
    INode (t, counts) kidCounts
  where
    kidCounts = tolCalcCounts kids
    counts = M.unionsWith (+) $
        M.singleton rank 1 : map (snd . iVal) (IM.elems kidCounts)

tolSummary :: Tol -> [BS.ByteString]
tolSummary tol = concat
    [ [ BS.intercalate "\t" $ map ("      " <>)
        ["", "", "P", "C", "O", "F", "G", "S"]
      , showNodeCounts lev0Counts
      , ""
      ]
    , map (("- " `BS.append`) . showNodeCounts) lev1Counts
    , [""]
    , filter (not . BS.null)
      [ BSC.unlines $ catMaybes
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
        IM.elems $ iKids lev0Counts

showNodeCounts :: INode (Taxon, Counts) -> BS.ByteString
showNodeCounts (INode (Taxon _ name, counts) _) =
    BS.concat [name, ":\t", showCounts counts]

lPadTo :: Char -> Int -> BS.ByteString -> BS.ByteString
lPadTo c n s = BSC.replicate (n - BS.length s) c <> s

showCounts :: M.Map Rank Int -> BS.ByteString
showCounts =
    BS.intercalate (BSC.pack "\t") .
    map (lPadTo ' ' 7 . BSC.pack . show . snd) .
    killSomeRanks .
    killFirstIfOne .
    M.toList
  where
    killFirstIfOne ((_, 1) : rest) = rest
    killFirstIfOne a = a
    killSomeRanks = filter ((`notElem` [Kingdom, Superfamily]) . fst)

rankWithMost
    :: BS.ByteString
    -> Rank
    -> Rank
    -> ITree (Taxon, Counts)
    -> BS.ByteString
rankWithMost name rank innerRank tolCounts =
    BSC.unlines $
        zipWith BS.append
            (label : repeat labelSp)
            results
  where
    label = BS.concat
        [ name, "   \t", rankPlural rank, " with most\t"
        , rankPlural innerRank, ":  \t"
        ]
    labelSp = BSC.map (\ c -> if isSpace c then c else ' ') label
    results = take 5 .
          map (\(name2, c) ->
              BS.concat [name2, BSC.pack (' ' : show c)]) .
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

onIVal :: (a -> a) -> INode a -> INode a
onIVal f (INode a kids) = INode (f a) kids

growTolAccum :: Rank -> Int -> Int -> INode Taxon -> IO (Int, INode Taxon)
growTolAccum !maxRank !growsLeft !nodeId
        !iNode@(INode (Taxon rank name) kids) = do
    if growsLeft <= 0 || rank >= maxRank
      then return (growsLeft, iNode)
      else if IM.null kids
        then do
          BSC.putStrLn $ BSC.pack (show rank) <> " " <> name <> " " <>
              BSC.pack (show nodeId)
          kids2 <- idGetTol nodeId
          let kids3 = if rank == Genus
                then IM.map
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
      else error "Species name didn't have genus as prefix."
  where
    genusSp = genus <> " "
