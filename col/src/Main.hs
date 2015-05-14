{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Data
import Data.Function
import Data.List
import Data.List.Ordered
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Numeric.SigFigs
import qualified System.Console.CmdLib as Cmd
import System.Console.Haskeline
import System.Directory

import Disp
import Rank
import Tol
import TolWork

data Opts = Opts
    { printTree     :: Bool
    , speciesCounts :: Bool
    , speciesMin    :: Int
    , speciesSigFigs :: Int
    , hideTaxonIds  :: Bool
    , collapseSuperfamilies :: Bool
    , treeSummary   :: Bool
    , growTree      :: Bool
    , growStep      :: Int
    , maxRank       :: String
    , findIdDupes   :: Bool
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
    (error "main unitialized field 11")
    (error "main unitialized field 12")

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
        , speciesSigFigs Cmd.%>
            [ Cmd.Help "Round species counts to this many significant digits."
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
        , growStep Cmd.%>
            -- No idea why default isn't already shown here:
            [ Cmd.Help $
              "Save the tree every time this many nodes have grown. " ++
              "(default: 100)"
            , Cmd.ArgHelp "N"
            , Cmd.Default (100 :: Int)
            ]
        , maxRank Cmd.%>
            [ Cmd.Help "Taxon rank depth to show or grow the tree to."
            , Cmd.ArgHelp "LEVEL"
            , Cmd.Default ("Species" :: String)
            ]
        , findIdDupes Cmd.%>
            [ Cmd.Help "Show any taxon ids used more than once."
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
        growTree growStep maxRank optExtra) =
    Opts printTree2 speciesCounts2 speciesMin treeSummary hideTaxonIds
        growTree2 growStep maxRank optExtra
  where
    speciesCounts2 = if speciesMin then True else speciesCounts
    printTree2 = if speciesCounts2 || hideTaxonIds then True else printTree
    growTree2 = if growStep then True else growTree
-}

idDupesMain :: FilePath -> Opts -> IO ()
idDupesMain tolF _ = do
    c <- BS.readFile tolF
    BS.putStr . BSC.unlines . map head . filter ((> 1) . length) .
        group . sort . map fst . map readIdTaxon .
        map (BSC.dropWhile (== ' ')) $ BSC.lines c

mainMain :: FilePath -> Opts -> IO ()
mainMain tolF opts = do
    tolFExists <- doesFileExist tolF
    tol <- if tolFExists
      then readTolF tolF
      else return M.empty
    let optMaxRank = readOrErr "Unknown taxon level" $ maxRank opts
        spCntShow = case speciesSigFigs opts of
          0 -> show
          n -> showSciSig n
    when (printTree opts) . BSL.putStr . BSL.fromChunks . map (<> "\n") .
        (if speciesCounts opts
          then
            showITree
                (\i (taxon, spCnt) ->
                    showIdTaxon (not $ hideTaxonIds opts) i taxon <> " " <>
                    BSC.pack (spCntShow spCnt))
                (flip compare `on` snd) .
            (if speciesMin opts == 0 then id
                else filterITree ((> speciesMin opts) . snd)) .
            mapITree (\(t, counts) ->
                (t, M.findWithDefault 0 Species counts)) .
            filterITree ((<= optMaxRank) . tRank . fst) .
            tolCalcCounts
          else
            showTol (not $ hideTaxonIds opts) .
            filterITree ((<= optMaxRank) . tRank)
        ) .
        (if collapseSuperfamilies opts
          then doCollapseSuperfamilies
          else id
        ) $
        tol
    when (treeSummary opts) . BS.putStr . BSC.unlines $ tolSummary tol
    when (growTree opts) $ growWhileCan optMaxRank opts tolF tol
    let countTol = tolCalcCounts tol
    unless (printTree opts || treeSummary opts || growTree opts) $
        runInputT defaultSettings $ exploreTol countTol []

showRankNum :: (Rank, Int) -> BS.ByteString
showRankNum (r, n) = BSC.pack (rankAbbr r : ':' : show n)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

exploreTol :: CountTol -> [TaxonId] -> InputT IO ()
exploreTol tol path = do
    let curTol = foldl' (\t p -> iKids . fromJust $ M.lookup p t) tol path
        (kids, vals) =
            unzip . sortOn (fmap negate . M.lookup Species . snd . snd) .
            map (second iVal) $ M.toList curTol
    lift . BS.putStr . BSC.unlines . renderCols $
        zipWith (\n (taxon, counts) -> BSC.pack (show n) : showTaxon taxon :
            map showRankNum (procCounts Kingdom counts)) [1 :: Int ..]
        vals
    inp <- getInputLine "number, (u)p, (q)uit> "
    case inp of
      Just "q" -> return ()
      Just "u" -> exploreTol tol (take (length path - 1) path)
      Just nStr -> case maybeRead nStr of
        Just n -> if n > 0 && n <= length kids
          then exploreTol tol (path ++ [kids !! (n - 1)])
          else exploreTol tol path
        _ -> exploreTol tol path
      _ -> exploreTol tol path

main :: IO ()
main = Cmd.getArgs >>= Cmd.executeR emptyOpts >>= \optsPre -> do
    let opts = {-optsDeps-} optsPre
    tolF <- case optExtra opts of
      [x] -> return x
      -- I guess Cmd doesn't expose its full help message to us? This is
      -- enough?
      _ -> error $ Cmd.mode_summary emptyOpts
    if findIdDupes opts
      then idDupesMain tolF opts
      else mainMain tolF opts

readOrErr :: Read a => String -> String -> a
readOrErr e s =
    fst . fromMaybe (error $ e ++ ": " ++ s) . listToMaybe $ reads s

growWhileCan :: Rank -> Opts -> FilePath -> Tol -> IO ()
growWhileCan optMaxRank opts tolF tol = do
    let numToGrow = growStep opts
    tol2 <- if M.null tol
      then error "FIXME: For now need to start with Kingdoms in file"
           -- idGetTol 0
      else return tol
    (growsLeft, tol3) <- growTol optMaxRank numToGrow $! tol2
    putStrLn "*"
    putStrLn "*"
    putStrLn "*"
    writeTolF (not $ hideTaxonIds opts) tolF $! tol3
    putStrLn $ "Saved.  growLeft = " ++ show growsLeft
    when (growsLeft < numToGrow) $ growWhileCan optMaxRank opts tolF tol3
