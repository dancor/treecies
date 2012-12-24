module Tol where

import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Either.Unwrap
import Data.List
import Data.List.Split
import Data.Ord
import Data.Tree

import Rank
import TolNode

type Fol = Forest TolNode

type Tol = Tree TolNode

kingdoms :: [String]
kingdoms =
    [ "Animalia"
    , "Plantae"
    , "Fungi"
    , "Protozoa"
    , "Bacteria"
    , "Chromista"
    , "Viruses"
    , "Archaea"
    ]

nodeToTree :: a -> Tree a
nodeToTree a = Node a []

drawTreeTiny :: Tree BS.ByteString -> [BS.ByteString]
drawTreeTiny (Node s kids) =
    [s] ++ map (BSC.cons ' ') (drawForestTiny kids)

drawForestTiny :: Forest BS.ByteString -> [BS.ByteString]
drawForestTiny = concatMap drawTreeTiny

showTolNode :: TolNode -> BS.ByteString
showTolNode (TolNode id name rank) =
    BSC.cons (rankAbbr rank) (BSC.cons ':' name)
    `BS.append`
    (' ' `BSC.cons` BSC.pack (show id))

showFol :: Fol -> [BS.ByteString]
showFol = drawForestTiny . map (fmap showTolNode)

readTolNode :: BS.ByteString -> TolNode
readTolNode s =
    TolNode (read $ BSC.unpack rest2) (BS.init rest1Sp) (abbrToRank r)
  where
    Just (r, s1) = BSC.uncons s
    Just (':', rest) = BSC.uncons s1
    (rest1Sp, rest2) = BSC.breakEnd (== ' ') rest

readTol :: [BS.ByteString] -> Tol
readTol (l:ls) =
    Node (readTolNode l) . readFol $ map BS.tail ls

readFol :: [BS.ByteString] -> Fol
readFol [] = []
readFol (l:ls) =
    readTol (l:ls1) : readFol ls2
  where
    (ls1, ls2) = span ((== ' ') . BSC.head) ls

readFolF :: FilePath -> IO Fol
readFolF folF = readFol . BSC.lines <$> BS.readFile folF

writeFolF :: FilePath -> Fol -> IO ()
writeFolF folF = BS.writeFile folF . BSC.unlines . showFol

treeKidCount :: Tree a -> Tree (a, Int)
treeKidCount (Node n kids) = Node (n, length kids) (forestKidCount kids)

forestKidCount :: Forest a -> Forest (a, Int)
forestKidCount = map treeKidCount

{-
folCalcCounts :: Fol -> Fol
folCalcCounts = map tolCalcCounts

tolCalcCounts :: Tol -> Tol
tolCalcCounts (Node tn []) = Node (tn {tnCount = Just 1}) []
tolCalcCounts (Node tn kids) =
    Node 
        (tn {tnCount = Just $ 
                sum (map (fromJust . tnCount . rootLabel) kids')
            })
        kids'
  where
    kids' = folCalcCounts kids

folExtractCounts :: Fol -> Forest (TolNode, Int)
folExtractCounts = map tolExtractCounts

tolExtractCounts :: Tol -> Tree (TolNode, Int)
tolExtractCounts = fmap (\tn -> (tn, fromJust $ tnCount tn))
-}

folSummary :: Fol -> [BS.ByteString]
folSummary fol = concat
    []
{-
    [ ["Levels:\t\t" ++ levShow levsTotal]
    , zipWith (\k levs -> "- " ++ k ++ ":\t" ++ levShow levs)
        kingdoms
        levsByKingdom ++
      ["Largest by immediate children:"]
    , concatMap (\(k, tol) -> 
          zipWith (\rank lev ->
                      "- " ++ k ++ "   \t" ++ rank ++ ":  \t" ++ 
                      showTnAndSize (maximumBy (comparing snd) lev)
                  )
              (init $ tail rankNames)
              (tail . levels $ treeKidCount tol)
          ) tolByKingdom
{-
    , ["Largest by leaf count (species or lowest taxon in tree):"]
    , concatMap (\(k, tol) -> 
          zipWith (\rank lev ->
                      "- " ++ k ++ "   \t" ++ rank ++ ":  \t" ++ 
                      showTnAndSize (maximumBy (comparing snd) lev)
                  )
              (init $ tail rankNames)
              (tail . levels . tolExtractCounts $ tolCalcCounts tol)
          ) tolByKingdom
-}
    ]
  where
    showTnAndSize (tn, n) = show n ++ " " ++ tnName tn
    tolByKingdom = zip kingdoms fol

    levShow = intercalate "\t" . map (show . length)
    levsByKingdom = map levels fol
    levsTotal = map concat $ transpose levsByKingdom
-}
