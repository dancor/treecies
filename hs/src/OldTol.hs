{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative
import Control.Arrow
import Data.List
import Data.List.Split
import Data.Tree
import qualified Data.Serialize as Ser
import qualified Data.ByteString as BS
import Data.Serialize.Derive
import GHC.Generics
import Data.Either.Unwrap

import Tol
import TolNode
import Rank

data OldTolNode = OldTolNode
    { otnId    :: Int
    , otnName  :: String
    , otnCount :: Maybe Int
    } deriving (Show, Generic)

instance Ser.Serialize OldTolNode where
    get = deriveGet
    put = derivePut

type OldFol = Forest OldTolNode

type OldTol = Tree OldTolNode

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

showOldFol :: OldFol -> [String]
showOldFol =
    map head . chunksOf 2 .
    lines . drawForest . map (fmap otnName)

oldFolToNew :: Rank -> OldFol -> Fol
oldFolToNew rank = map (oldTolToNew rank)

oldTolToNew :: Rank -> OldTol -> Tol
oldTolToNew rank (Node (OldTolNode id name _) kids) = 
    Node (TolNode id name rank') $ oldFolToNew (succ rank') kids
  where
    rank' =
        if rank == Superfamily && (not $ "oidea" `isSuffixOf` name)
            then Family
            else rank

main = do
    oldFol <- fromRight . Ser.decode <$> BS.readFile "2012-12-21"
    let fol = oldFolToNew Kingdom oldFol
    {-
    putStr . unlines $ showOldFol oldFol
    putStr . unlines . showFol $ oldFolToNew Kingdom oldFol
    putStr . unlines . lines .
        drawForest . map (fmap show) $ oldFolToNew Kingdom oldFol
    -}
    writeFol "2012-12-21.new" fol
