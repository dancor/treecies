{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Data
import Data.List
import qualified Data.IntMap as IM
import Data.Maybe
import qualified System.Console.CmdLib as Cmd
import System.Directory

import Tol
import TolWork
import Web

data Opts = Opts
    { printTree    :: Bool
    , speciesCount :: Bool
    , speciesMin   :: Int
    , treeSummary  :: Bool
    , growTree     :: Bool
    , growRank     :: String
    , growStep     :: Int
    , optExtra     :: [String]
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

instance Cmd.Attributes Opts where
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

instance Cmd.RecordCommand Opts where
    mode_summary _ = intercalate "\n"
        [ "Usage: ./web-tol [options] <tree-file-to-create-or-add-to.txt>"
        , ""
        , "Create or manipulate a taxonomy tree."
        ]
    run' _ _ = return ()

{-
filterTol :: Opts -> Tree (t, Map.Map Rank Int) -> Tree (t, Map.Map Rank Int)
filterTol opts (Node a kids) = Node a $ filterFol opts kids

filterFol :: Opts -> Forest (t, Map.Map Rank Int) -> Forest (t, Map.Map Rank Int)
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
-}

main :: IO ()
main = Cmd.getArgs >>= Cmd.executeR emptyOpts >>= \opts -> do
    tolF <- case optExtra opts of
      [x] -> return x
      -- I guess Cmd doesn't expose its full help message to us? This is
      -- enough?
      _ -> error $ Cmd.mode_summary emptyOpts
    tolFExists <- doesFileExist tolF
    tol <- if tolFExists
      then readTolF tolF
      else return IM.empty
    when (printTree opts) . BSL.putStr . BSL.fromChunks $
        if speciesCount opts
          then
            error "todo"
            -- showFolSp . filterFol opts $ folCalcCounts fol
          else showTol tol
    when (treeSummary opts) . BS.putStr . BSC.unlines $
        error "todo"
        -- tolSummary tol
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
    writeTolF tolF tol3
    putStrLn $ "Saved.  growLeft = " ++ show growsLeft
    when (growsLeft < numToGrow) $ growWhileCan opts tolF tol3
