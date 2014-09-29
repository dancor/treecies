{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Char8 as BSC
import qualified Data.IntMap as IM
import Data.Maybe
import Network.HTTP.Conduit
import System.IO
import qualified Text.XML.Light as X

import Tol

pullCdData :: X.Element -> Maybe String
pullCdData el = fmap X.cdData . listToMaybe . X.onlyText $ X.elContent el

pullCdDataOf :: String -> X.Element -> Maybe String
pullCdDataOf a = pullCdData . fromJust . X.findChild (X.unqual a)

xmlGetTaxon :: X.Element -> Maybe (Int, INode Taxon)
xmlGetTaxon a = do
    taxonId <- read <$> pullCdDataOf "id" a
    rank <- read <$> pullCdDataOf "rank" a
    let name = BSC.pack $ fromMaybe "" (pullCdDataOf "name" a)
    return (taxonId, INode (Taxon rank name) IM.empty)

idGetTol :: Int -> IO Tol
idGetTol taxonId = catch (idGetTolTry taxonId) $ \e -> do
    hPutStrLn stderr $ show (e :: IOException)
    idGetTolTryAgain taxonId

idGetTolTryAgain :: Int -> IO Tol
idGetTolTryAgain taxonId = do
    hPutStrLn stderr "Waiting 1 sec.."
    threadDelay (1 * 1000 * 1000)
    idGetTol taxonId

el1Mb :: [a] -> Maybe a
el1Mb = listToMaybe . drop 1

idGetXml :: Int -> IO [X.Content]
idGetXml taxonId = do
    xmlStr <- simpleHttp $
        "http://www.catalogueoflife.org/col/webservice?response=full&" ++
        "id=" ++ show taxonId
    return $ X.parseXML xmlStr

idGetTolTry :: Int -> IO Tol
idGetTolTry taxonId = do
    let myHandler (_e :: HttpException) = return Nothing
    xmlMb <- catch (Just <$> idGetXml taxonId) myHandler
    case xmlMb of
      Nothing -> idGetTolTryAgain taxonId
      Just xml -> do
        let results =
                fmap (X.onlyElems . X.elContent) . el1Mb $ X.onlyElems xml
            resultToTol :: [X.Element] -> Maybe Tol
            resultToTol [] = Nothing
            resultToTol rs@(r:_) =
                fmap IM.fromList . mapM xmlGetTaxon $ if taxonId == 0
                  then rs
                  else
                    X.findChildren (X.unqual "taxon") . fromJust $
                    X.findChild (X.unqual "child_taxa") r
        case results of
          Nothing -> idGetTolTryAgain taxonId
          Just x -> case resultToTol x of
            Just x2 -> return x2
            Nothing -> error $ "Failed on taxonId: " ++ show taxonId
