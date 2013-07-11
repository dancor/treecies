{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Data
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Tree
import qualified System.Console.CmdLib as Cmd
import System.Directory

import Rank
import SearchInfo
import TolNode
import Tol

growTol :: Int -> Rank -> Tol -> IO (Int, Tol)
growTol 0 _rank tol = return (0, tol)
growTol maxGrows rank (n@(Node tn [])) =
    if tnRank tn >= rank
    then return (maxGrows, n)
    else print tn >> (,) (maxGrows - 1) <$> tnGrabKids tn
growTol maxGrows rank (n@(Node tn fol)) =
    if tnRank tn >= rank
    then return (maxGrows, n)
    else second (Node tn) <$> growFol maxGrows rank fol

growFol :: Int -> Rank -> Fol -> IO (Int, Fol)
growFol maxGrows _ [] = return (maxGrows, [])
growFol maxGrows rank (tol:fol) = do
    (maxGrows', tol') <- growTol maxGrows rank tol
    (maxGrows'', fol') <- growFol maxGrows' rank fol
    return (maxGrows'', tol':fol')

readOrErr :: Read a => String -> String -> a
readOrErr e s =
    fst . fromMaybe (error $ e ++ ": " ++ s) . listToMaybe $ reads s

growWhileCan :: Main -> String -> Fol -> IO ()
growWhileCan opts folF fol = do
    let numToGrow = growStep opts
    (growLeft, fol') <- growFol numToGrow
        (readOrErr "Unknown taxon level" $ growRank opts) fol
    putStrLn "*"
    putStrLn "*"
    putStrLn "*"
    writeFolF folF fol'
    putStrLn $ "Saved.  growLeft = " ++ show growLeft
    when (growLeft < numToGrow) $ growWhileCan opts folF fol'

data Main = Main
    { printTree    :: Bool
    , speciesCount :: Bool
    , speciesMin   :: Int
    , treeSummary  :: Bool
    , growTree     :: Bool
    , growRank     :: String
    , growStep     :: Int
    , optExtra     :: [String]
    } deriving (Data, Eq, Show, Typeable)

emptyMain :: Main
emptyMain = Main
    (error "main unitialized field 1")
    (error "main unitialized field 2")
    (error "main unitialized field 3")
    (error "main unitialized field 4")
    (error "main unitialized field 5")
    (error "main unitialized field 6")
    (error "main unitialized field 7")
    (error "main unitialized field 8")

instance Cmd.Attributes Main where
    attributes _ = Cmd.group "Options"
        [ printTree Cmd.%>
            [ Cmd.Help "Output the tree."
            ]
        , speciesCount Cmd.%>
            [ Cmd.Help "Show species counts."
            ]
        , speciesMin Cmd.%>
            [ Cmd.Help "Only print nodes with at least this many species."
            , Cmd.Default (0 :: Int)
            ]
        , treeSummary Cmd.%>
            [ Cmd.Help "Summarize the contents of the tree."
            ]
        , growTree Cmd.%>
            [ Cmd.Help $ "Grow the tree to the grow level."
            ]
        , growRank Cmd.%>
            [ Cmd.Help "Taxon rank to grow the tree to."
            , Cmd.ArgHelp "LEVEL"
            , Cmd.Default ("Species" :: String)
            ]
        , growStep Cmd.%>
            -- No idea why default isn't already shown here:
            [ Cmd.Help $
              "Save the tree every time this many nodes have grown. " ++
              "(default: 100)"
            , Cmd.ArgHelp "N"
            , Cmd.Default (100 :: Int)
            ]
        , optExtra Cmd.%>
            [ Cmd.Extra True
            ]
        ]

instance Cmd.RecordCommand Main where
    mode_summary _ = intercalate "\n"
        [ "Usage: ./web-tol [options] <tree-file-to-create-or-add-to.txt>"
        , ""
        , "Create or manipulate a taxonomy tree."
        ]
    run' _ _ = return ()

filterTol :: Main -> Tree (t, Map.Map Rank Int) -> Tree (t, Map.Map Rank Int)
filterTol opts (Node a kids) = Node a $ filterFol opts kids

filterFol :: Main -> Forest (t, Map.Map Rank Int) -> Forest (t, Map.Map Rank Int)
filterFol opts = map (filterTol opts) . filter filtF
  where
    filtF (Node (_, counts) _) =
        speciesMin opts <= Map.findWithDefault 0 Species counts

showFolSp :: Forest (TolNode, Map.Map Rank Int) -> [BS.ByteString]
showFolSp = drawForestTiny . map (fmap showTolNodeSp)

showTolNodeSp :: (TolNode, Map.Map Rank Int) -> BS.ByteString
showTolNodeSp (TolNode _ name rank, counts) =
    BSC.cons (rankAbbr rank) (BSC.cons ':' name)
    `BS.append`
    (' ' `BSC.cons` BSC.pack (show $ Map.findWithDefault 0 Species counts))

main :: IO ()
main = Cmd.getArgs >>= Cmd.executeR emptyMain >>= \opts -> do
    folF <- case optExtra opts of
      [x] -> return x
      -- I guess Cmd doesn't expose its full help message to us? This is
      -- enough?
      _ -> error $ Cmd.mode_summary emptyMain
    folFExists <- doesFileExist folF
    fol <- if folFExists
      then readFolF folF
      else return initialFol
    when (printTree opts) . BSL.putStr . BSL.fromChunks $
        if speciesCount opts
          then showFolSp . filterFol opts $ folCalcCounts fol
          else showFol fol
    when (treeSummary opts) . BS.putStr . BSC.unlines $ folSummary fol
    when (growTree opts) $ growWhileCan opts folF fol
