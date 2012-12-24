module SearchInfo where

import Control.Applicative
import qualified Data.ByteString.Char8 as BSC
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Tree
import Network.HTTP
import qualified Text.XML.Light as X

import Rank
import TolNode
import Tol

data SearchInfo = SearchInfo
    { siTolNode :: TolNode
    , siKids :: [TolNode]
    }

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

idToInfo :: Int -> IO SearchInfo
idToInfo id = searchToInfo Nothing $ "id=" ++ show id

urlize :: String -> String
urlize = replace " " "%s"

topNameToInfo :: String -> IO SearchInfo
topNameToInfo name = searchToInfo (Just Kingdom) $ "name=" ++ urlize name

pullCdData :: X.Element -> String
pullCdData = X.cdData . head . X.onlyText . X.elContent

pullCdDataOf :: String -> X.Element -> String
pullCdDataOf a = pullCdData . fromJust . X.findChild (X.unqual a)

xmlGetTolNode :: X.Element -> TolNode
xmlGetTolNode a = TolNode
    (read $     pullCdDataOf "id" a)
    (BSC.pack $ pullCdDataOf "name" a)
    (read $     pullCdDataOf "rank" a)

searchToInfo :: Maybe Rank -> String -> IO SearchInfo
searchToInfo rankMb search = do
    xml <- X.parseXML <$> openURL (
        "http://www.catalogueoflife.org/col/webservice?response=full&" ++
        search)
    let results = X.onlyElems . X.elContent . (!! 1) $ X.onlyElems xml
        resultToInfo :: X.Element -> SearchInfo
        resultToInfo r =
            SearchInfo (xmlGetTolNode r) kids
          where
            kids = map xmlGetTolNode . X.findChildren (X.unqual "taxon") .
                fromJust $ X.findChild (X.unqual "child_taxa") r
        resultFilter =
            maybe id (\r -> filter ((r ==) . tnRank . siTolNode)) rankMb
    return . head . resultFilter $ map resultToInfo results

searchInfoToTol :: SearchInfo -> Tol
searchInfoToTol (SearchInfo tolNode kids) =
    Node tolNode $ map nodeToTree kids

idToTol :: Int -> IO Tol
idToTol id = searchInfoToTol <$> idToInfo id

initialFol :: IO Fol
initialFol = map searchInfoToTol <$> mapM topNameToInfo kingdoms

