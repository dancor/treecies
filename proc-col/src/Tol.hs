{-# LANGUAGE OverloadedStrings #-}

module Tol where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Tree

import Rank
import TolNode

type Fol = Forest TolNode

type Tol = Tree TolNode

nodeToTree :: a -> Tree a
nodeToTree a = Node a []

initialFol :: Fol
initialFol = [nodeToTree initialTolNode]

drawTreeTiny :: Tree BS.ByteString -> [BS.ByteString]
drawTreeTiny (Node s kids) =
    (BS.append s "\n") : map (BSC.cons ' ') (drawForestTiny kids)

drawForestTiny :: Forest BS.ByteString -> [BS.ByteString]
drawForestTiny = concatMap drawTreeTiny

showTolNode :: TolNode -> BS.ByteString
showTolNode (TolNode cladeId name rank) = BS.concat
    [ BSC.cons (rankAbbr rank) $ BSC.cons ':' name
    , BSC.cons ' ' . BSC.pack $ show cladeId
    ]

readTolNode :: BS.ByteString -> TolNode
readTolNode s =
    TolNode (read $ BSC.unpack rest2) (BS.init rest1Sp) (abbrToRank r)
  where
    Just (r, s1) = BSC.uncons s
    Just (':', rest) = BSC.uncons s1
    (rest1Sp, rest2) = BSC.breakEnd (== ' ') rest

showFol :: Fol -> [BS.ByteString]
showFol = drawForestTiny . map (fmap showTolNode)

readTol :: [BS.ByteString] -> Tol
readTol (l:ls) =
    Node (readTolNode l) . readFol $ map BS.tail ls
readTol [] = error "readTol: empty (todo or impossible?)"

readFol :: [BS.ByteString] -> Fol
readFol [] = []
readFol (l:ls) =
    readTol (l:ls1) : readFol ls2
  where
    (ls1, ls2) = span ((== ' ') . BSC.head) ls

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

readFolF :: FilePath -> IO Fol
readFolF folF = do
    fol <- readFol . BSC.lines <$> BS.readFile folF
    --let (fol', dupes) = folFindDupes fol
    --mapM_ print dupes
    --return fol'
    return fol

writeFolF :: FilePath -> Fol -> IO ()
writeFolF folF = BSL.writeFile folF . BSL.fromChunks . showFol

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
