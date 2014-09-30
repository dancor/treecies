{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Data
import Data.Function
import qualified Data.IntMap as IM
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified System.Console.CmdLib as Cmd
import System.Directory

import Rank
import Tol
import TolWork
import Web

data Opts = Opts
    { printTree     :: Bool
    , speciesCounts :: Bool
    , speciesMin    :: Int
    , hideTaxonIds  :: Bool
    , collapseSuperfamilies :: Bool
    , treeSummary   :: Bool
    , growTree      :: Bool
    , growRank      :: String
    , growStep      :: Int
    , optExtra      :: [String]
    } deriving (Data, Eq, Show, Typeable)

emptyOpts :: Opts
emptyOpts = Opts
    (error "main unitialized field 1")
    (error "main unitialized field 2")
    (error "main unitialized field 3")
    (error "main unitialized field 4")
    (error "main unitialized field 5")
    (error "main unitialized field 6")
    (error "main unitialized field 7")
    (error "main unitialized field 8")
    (error "main unitialized field 9")
    (error "main unitialized field 10")

instance Cmd.Attributes Opts where
    attributes _ = Cmd.group "Options"
        [ printTree Cmd.%>
            [ Cmd.Help "Output the tree."
            ]
        , speciesCounts Cmd.%>
            [ Cmd.Help "Show species counts."
            ]
        , speciesMin Cmd.%>
            [ Cmd.Help "Only print nodes with at least this many species."
            , Cmd.Default (0 :: Int)
            ]
        , hideTaxonIds Cmd.%>
            [ Cmd.Help "Don't show the internal taxon ids."
            ]
        , collapseSuperfamilies Cmd.%>
            [ Cmd.Help "Collapse superfamilies."
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

instance Cmd.RecordCommand Opts where
    mode_summary _ = intercalate "\n"
        [ "Usage: ./web-tol [options] <tree-file-to-create-or-add-to.txt>"
        , ""
        , "Create or manipulate a taxonomy tree."
        ]
    run' _ _ = return ()

{-
optsDeps :: Opts -> Opts
optsDeps (Opts printTree speciesCounts speciesMin treeSummary hideTaxonIds
        growTree growRank growStep optExtra) =
    Opts printTree2 speciesCounts2 speciesMin treeSummary hideTaxonIds
        growTree2 growRank growStep optExtra
  where
    speciesCounts2 = if speciesMin then True else speciesCounts
    printTree2 = if speciesCounts2 || hideTaxonIds then True else printTree
    growTree2 = if growRank || growStep then True else growRank
-}

main :: IO ()
main = Cmd.getArgs >>= Cmd.executeR emptyOpts >>= \optsPre -> do
    let opts = {-optsDeps-} optsPre
    tolF <- case optExtra opts of
      [x] -> return x
      -- I guess Cmd doesn't expose its full help message to us? This is
      -- enough?
      _ -> error $ Cmd.mode_summary emptyOpts
    tolFExists <- doesFileExist tolF
    tol <- if tolFExists
      then readTolF tolF
      else return IM.empty
    when (printTree opts) . BSL.putStr . BSL.fromChunks . map (<> "\n") $
        (if speciesCounts opts
          then
            showITree
                (\i (taxon, spCnt) ->
                    showIdTaxon (not $ hideTaxonIds opts) i taxon <> " " <>
                    BSC.pack (show spCnt))
                (flip compare `on` snd) .
            (if speciesMin opts == 0 then id
                else filterITree ((> speciesMin opts) . snd)) .
            mapITree (\(t, counts) ->
                (t, M.findWithDefault 0 Species counts)) .
            tolCalcCounts
          else showTol (not $ hideTaxonIds opts)
        )
        (if collapseSuperfamilies opts
          then doCollapseSuperfamilies tol
          else tol
        )
    when (treeSummary opts) . BS.putStr . BSC.unlines $ tolSummary tol
    when (growTree opts) $ growWhileCan opts tolF tol

readOrErr :: Read a => String -> String -> a
readOrErr e s =
    fst . fromMaybe (error $ e ++ ": " ++ s) . listToMaybe $ reads s

growWhileCan :: Opts -> FilePath -> Tol -> IO ()
growWhileCan opts tolF tol = do
    let numToGrow = growStep opts
    tol2 <- if IM.null tol
      then idGetTol 0
      else return tol
    (growsLeft, tol3) <- growTol
        (readOrErr "Unknown taxon level" $ growRank opts)
        numToGrow
        tol2
    putStrLn "*"
    putStrLn "*"
    putStrLn "*"
    writeTolF (not $ hideTaxonIds opts) tolF tol3
    putStrLn $ "Saved.  growLeft = " ++ show growsLeft
    when (growsLeft < numToGrow) $ growWhileCan opts tolF tol3
