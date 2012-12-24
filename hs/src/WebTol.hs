{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List
import Data.Maybe
import Data.Ord
import Data.Tree
import FUtil
import qualified System.Console.CmdLib as Cmd
import System.Console.CmdLib hiding (group, Default)
import System.Directory
import System.IO

import Rank
import SearchInfo
import TolNode
import Tol

growTol :: Int -> Rank -> Tol -> IO (Int, Tol)
growTol 0 _rank tol = return (0, tol)
growTol maxGrows rank (n@(Node tn [])) = 
    if tnRank tn >= rank
    then return (maxGrows, n)
    else do
        print tn
        (,) (maxGrows - 1) <$> idToTol (tnId tn)
growTol maxGrows rank (n@(Node tn fol)) =
    if tnRank tn >= rank
    then return (maxGrows, n)
    else second (Node tn) <$> growFol maxGrows rank fol

growFol :: Int -> Rank -> Fol -> IO (Int, Fol)
growFol maxGrows rank [] = return (maxGrows, [])
growFol maxGrows rank (tol:fol) = do
    (maxGrows', tol') <- growTol maxGrows rank tol
    (maxGrows'', fol') <- growFol maxGrows' rank fol
    return (maxGrows'', tol':fol')

readOrErr :: Read a => String -> String -> a
readOrErr e s =
    fst . fromMaybe (error $ e ++ ": " ++ s) . listToMaybe $ reads s

growWhileCan :: Main -> String -> Fol -> IO ()
growWhileCan opts folF fol = do
    (growLeft, fol') <- growFol (growStep opts)
        (readOrErr "Unknown taxon level" $ growRank opts) fol
    let folEnc = BSC.unlines $ showFol fol'
    BS.writeFile folF folEnc
    BS.writeFile (folF ++ ".lol") folEnc
    putStrLn $ "Saved.  growLeft = " ++ show growLeft
    when (growLeft == 0) $ growWhileCan opts folF fol'

data Main = Main
    { printTree :: Bool
    , treeSummary :: Bool
    , growTree :: Bool
    , growRank :: String
    , growStep :: Int
    , optExtra :: [String]
    } deriving (Data, Eq, Typeable)

instance Attributes Main where
    attributes _ = Cmd.group "Options"
        [ printTree %>
            [ Help "Output the tree."
            ]
        , treeSummary %>
            [ Help "Summarize the contents of the tree."
            ]
        , growTree %>
            [ Help $ "Grow the tree (one level closer, currently)\n" ++ 
                     "to the grow level."
            ]
        , growRank %>
            [ Help "Taxon rank to grow the tree to."
            , ArgHelp "LEVEL"
            , Cmd.Default ("Species" :: String)
            ]
        , growStep %>
            [ Help "Save the tree every time this many nodes have grown."
            , ArgHelp "N"
            , Cmd.Default (100 :: Int)
            ]
        , optExtra %>
            [ Extra True
            ]
        ]

instance RecordCommand Main where
    mode_summary _ = intercalate "\n"
        [ "Usage: ./web_tol [options] file"
        , ""
        , "Manipulate a taxonomic tree."
        , "If file doesn't exist, a phylum level tree is initially populated."
        ]

main :: IO ()
main = getArgs >>= executeR Main {} >>= \opts -> do
    let [folF] = optExtra opts
    unlessM (doesFileExist folF) $ do
        error "XXX: switch to id=0 top node"
        {-
        putStrLn $ "Grabbing phylum tree to file: " ++ show folF
        fol <- initialFol
        writeFolF folF fol
        putStrLn "Done."
        -}
    fol <- readFolF folF
    when (printTree opts) . BS.putStr . BSC.unlines $ showFol fol
    when (treeSummary opts) . BS.putStr . BSC.unlines $ folSummary fol
    when (growTree opts) $ growWhileCan opts folF fol
