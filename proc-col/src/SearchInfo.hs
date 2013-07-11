module SearchInfo where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List.Utils
import Data.Maybe
import Data.Tree
import Network.HTTP
import Prelude hiding (catch)
import System.IO
import qualified Text.XML.Light as X

import Rank
import TolNode
import Tol

data SearchInfo = SearchInfo
    { siTolNode :: TolNode
    , siKids :: [TolNode]
    }

openUrl :: String -> IO String
openUrl x = getResponseBody =<< simpleHTTP (getRequest x)

idToInfo :: Int -> IO SearchInfo
idToInfo cladeId = searchToInfo Nothing $ "id=" ++ show cladeId

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
searchToInfo rankMb search = catch (searchToInfoTry rankMb search) $ \e -> do
    hPutStrLn stderr $ show (e :: IOException)
    searchToInfoTryAgain rankMb search

searchToInfoTryAgain :: Maybe Rank -> String -> IO SearchInfo
searchToInfoTryAgain rankMb search = do
    hPutStrLn stderr "Waiting 1 sec.."
    threadDelay (1 * 1000 * 1000)
    searchToInfo rankMb search

searchToInfoTry :: Maybe Rank -> String -> IO SearchInfo
searchToInfoTry rankMb search = do
    xmlStr <- openUrl $
        "http://www.catalogueoflife.org/col/webservice?response=full&" ++
        search
    let tryGetEl1 x = case x of
          _:y:_ -> Just y
          _ -> 
            Nothing
            -- error $ "tryGetEl1: " ++ show (rankMb, search, x, xml)
        results =
            fmap (X.onlyElems . X.elContent) . tryGetEl1 . X.onlyElems $
            X.parseXML xmlStr
        resultToInfo :: X.Element -> SearchInfo
        resultToInfo r =
            SearchInfo (xmlGetTolNode r) kids
          where
            kids = map xmlGetTolNode . X.findChildren (X.unqual "taxon") .
                fromJust $ X.findChild (X.unqual "child_taxa") r
        resultFilter =
            maybe id (\r -> filter ((r ==) . tnRank . siTolNode)) rankMb
    case results of
      Nothing -> searchToInfoTryAgain rankMb search
      Just x -> return . head . resultFilter $ map resultToInfo x

searchInfoToTol :: SearchInfo -> Tol
searchInfoToTol (SearchInfo tolNode kids) =
    Node tolNode $ map nodeToTree kids

tnGrabKids :: TolNode -> IO Tol
tnGrabKids tn =
    speciesTrims . searchInfoToTol <$> idToInfo (tnId tn)
  where
    speciesTrims (Node tn2 kids) = Node tn2 $ map speciesTrim kids
    speciesTrim (n@(Node tn3 [])) =
      if tnRank tn3 == Species
          then let parentNameSp = tnName tn `BS.append` BSC.singleton ' '
                   newName = BS.drop (BS.length parentNameSp) $ tnName tn3
               in  if parentNameSp `BS.isPrefixOf` tnName tn3
                   then Node (tn3 {tnName = newName}) []
                   else error "Species name didn't have parent as prefix."
          else n
    speciesTrim (Node _ _) = error "Grabbed just kids but now a grandparent."

{-
initialFol :: IO Fol
initialFol = map searchInfoToTol <$> mapM (topNameToInfo . BSC.unpack) kingdoms
-}
