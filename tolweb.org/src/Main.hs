{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.List
import Data.Maybe
import Data.String.Utils
import System.Environment
import System.IO
import Text.XML.Expat.Tree

import Debug.Trace

data ProcStats = ProcStats
    { psMaxDepth :: Int
    , psNumBlank :: Int
    }

mergeProcStats :: ProcStats -> ProcStats -> ProcStats
mergeProcStats (ProcStats d1 n1) (ProcStats d2 n2) =
    ProcStats (max d1 d2) (n1 + n2)

type ProcRet = ([BS.ByteString], ProcStats)

mergeProcRet :: ProcRet -> ProcRet -> ProcRet
mergeProcRet (o1, s1) (o2, s2) = (o1 ++ o2, mergeProcStats s1 s2)

-- | Process a clade <NODE>.
-- This is the main processing function.
-- Return a list to keep things lazy.
procATolNode :: Int -> UNode BS.ByteString -> ProcRet
procATolNode depth unode =
    -- There will one <NODES> or none (for a leaf clade).
    case listToMaybe $ filter (isNamed "NODES") (eChildren unode) of
      Nothing -> ([curOutputLine], ProcStats depth curNumBlank)
      Just theNodes ->
          ( curOutputLine : childrenOutput
          , childrenStats
                { psNumBlank = psNumBlank childrenStats + curNumBlank
                }
          )
        where
          (childrenOutput, childrenStats) = procATolNodes (depth + 1) theNodes
  where
    cladeIsExtinct = maybe
        (trace "Missing EXTINCT attribute: assuming not extinct." False)
        -- Tol's magic number for extict clades:
        (== "2") $
        lookup "EXTINCT" (eAttributes unode)
    cladeName = maybe
        (trace "Missing NAME attribute." "?")
        (BS.concat . map getText . eChildren) .
        listToMaybe $ filter (isNamed "NAME") (eChildren unode)
    cladeDescription =
        -- Data weirdness:
        (\x -> if x == "."
            then
              trace 
              ("Just a dot for DESCRIPTION text: " ++ BSC.unpack cladeName)
              ""
            else x) .
        (\x -> if "\n" `BS.isInfixOf` x
            then trace ("Newline in DESCRIPTION text: " ++ show x) .
                 BSC.pack . replace "\n" " " . replace " \n" " " $
                 BSC.unpack x
            else x) .
        -- Lots of these..
        (\x -> if x == "0" then "" else x) .
        -- These all seem to be (styled HTML..) references,
        -- which I think are actually supposed to be in AUTHORITY
        -- or something. We don't want them.
        -- There are other references that are harder to filter, oh well.
        (\x -> if "&" `BS.isPrefixOf` x then "" else x) .
        maybe
            (trace "Missing DESCRIPTION attribute." "")
            (BS.concat . map getText . eChildren) .
        listToMaybe $ filter (isNamed "DESCRIPTION") (eChildren unode)
    curOutputLine = BS.concat
        [ BSC.pack . reverse . take depth $
          ' ' : reverse (show depth) ++ cycle " "
        , if cladeIsExtinct then "*" else ""
        , if BS.null cladeName then "." else cladeName
        , if BS.null cladeDescription
            then ""
            else BS.append ": " cladeDescription
        ]
    curNumBlank :: Int
    curNumBlank = if BS.null cladeName then 1 else 0

-- | Process a UNode corresponding to a <NODES> or the top-level <TREE>.
-- The top-level <TREE> is just like a <NODES>:
-- in both cases, we want the <NODE ..>..</NODE>'s inside.
-- Return a list to keep things lazy.
procATolNodes :: Int -> UNode BS.ByteString -> ProcRet
procATolNodes depth unode = foldl1' mergeProcRet .
    map (procATolNode depth) $
    filter (isNamed "NODE") (eChildren unode)

-- | The top-level XML UNode is the top-level <TREE>,
-- so process with 'procATolNodes'.
procXml :: UNode BS.ByteString -> ProcRet
procXml xml = procATolNodes 0 xml

main :: IO ()
main = do
    args <- getArgs
    fName <- case args of
      [x] -> return x
      _ -> error "Usage: ./proc-tolweb.org <tolweb.org-tree.xml>"
    (xml, xmlErrMb) <- parse defaultParseOptions <$> BSL.readFile fName
    case xmlErrMb of
      Just err -> error ("XML parse failed: " ++ show err)
      _ -> do
        let (ls, ProcStats maxDepth blankNum) = procXml xml
        hPutStrLn stderr $ "Max depth: " ++ show maxDepth
        hPutStrLn stderr $ "Number of unnamed clades: " ++ show blankNum
        mapM_ BSC.putStrLn ls
