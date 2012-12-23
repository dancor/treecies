#include <h>

import Network.HTTP
import qualified Data.Serialize as Ser
import qualified Text.XML.Light as X

data TolNode = TolNode
    { tnId    :: Int
    , tnName  :: String
    , tnCount :: Maybe Int
    } deriving (Show, Generic)

instance Ser.Serialize TolNode where
    get = deriveGet
    put = derivePut

type Fol = Forest TolNode

type Tol = Tree TolNode

type Taxon = [String]

type SearchInfo = ((Int, String), [(Int, String)])

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

taxonLevels :: [String]
taxonLevels =
    [ "Kingdom"
    , "Phylum"
    , "Class"
    , "Order"
    , "Family"
    , "Genus"
    ]

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

idToInfo :: Int -> IO SearchInfo
idToInfo id = searchToInfo Nothing $ "id=" ++ show id

urlize :: String -> String
urlize = replace " " "%s"

topNameToInfo :: String -> IO SearchInfo
topNameToInfo name = searchToInfo (Just "Kingdom") $ "name=" ++ urlize name

pullCdData :: X.Element -> String
pullCdData = X.cdData . head . X.onlyText . X.elContent

pullCdDataOf :: String -> X.Element -> String
pullCdDataOf a = pullCdData . fromJust . X.findChild (X.unqual a)

grabIdAndName :: X.Element -> (Int, String)
grabIdAndName a = (read $ pullCdDataOf "id" a, pullCdDataOf "name" a)

searchToInfo :: Maybe String -> String -> IO SearchInfo
searchToInfo rankMb search = do
    xml <- X.parseXML <$> openURL (
        "http://www.catalogueoflife.org/col/webservice?response=full&" ++
        search)
    let results = X.onlyElems . X.elContent . (!! 1) $ X.onlyElems xml
        resultToInfo r =
           (rank, (idAndName, kids))
          where
            idAndName = grabIdAndName r
            rank = pullCdDataOf "rank" r
            kids =
                map grabIdAndName . X.findChildren (X.unqual "taxon") .
                fromJust $ X.findChild (X.unqual "child_taxa") r
        resultFilter = maybe id (\ r -> filter ((r ==) . fst)) rankMb
    return . snd . head . resultFilter $ map resultToInfo results

idAndNameToNode :: Int -> String -> TolNode
idAndNameToNode id name = TolNode id name Nothing

nodeToTree :: a -> Tree a
nodeToTree a = Node a []

searchInfoToTol :: SearchInfo -> Tol
searchInfoToTol ((id, name), kids) =
    Node (idAndNameToNode id name) $
        map (nodeToTree . uncurry idAndNameToNode) kids

idToTol :: Int -> IO Tol
idToTol id = searchInfoToTol <$> idToInfo id

initialFol :: IO Fol
initialFol = map searchInfoToTol <$> mapM topNameToInfo kingdoms

showFol =
    putStr . unlines . map head . chunksOf 2 .
    lines . drawForest . map (fmap tnName)

growTol :: Int -> Tol -> IO Tol
growTol 0 tol = return tol
growTol toLevel (Node a []) = do
    print a
    idToTol $ tnId a
growTol toLevel (Node a fol) = Node a <$> growFol (toLevel - 1) fol

growFol :: Int -> Fol -> IO Fol
growFol toLevel = mapM (growTol toLevel)

main :: IO ()
main = do
    [folF] <- getArgs
    unlessM (doesFileExist folF) $ do
        fol <- initialFol
        BS.writeFile folF $ Ser.encode fol
    fol <- fromRight . Ser.decode <$> BS.readFile folF
    showFol fol
    fol' <- growFol 2 fol
    BS.writeFile folF $ Ser.encode fol'

-- taxonToSearchUrl :: Taxon -> String
-- taxonToSearchUrl taxon =
--   "http://www.catalogueoflife.org/col/browse/classification/" ++
--   intercalate "/" (zipWith (\ n t -> urlize n ++ "/" ++ urlize t)
--   taxonLevels taxon) ++
--   "/match/1"
--
-- taxonToSpeciesCount :: [String] -> IO Int
-- taxonToSpeciesCount [] = error "Total species count requested."
-- taxonToSpeciesCount taxon = do
--   ls <- lines <$> openURL (taxonToSearchUrl taxon)
--   let
--     line = head . drop 1 $ dropWhile (not . (isPrefixOf "Records found:")) ls
--     word = head . drop 1 $ words line
--     num = read . filter (/= ',') $ drop 1 word
--   return num
--
-- growTol :: Taxon -> Int -> Tol -> IO Tol
-- growTol _ 0 tol = return tol
-- growTol parent toLevel subTrees = forM subTrees $ \ (Node (s, mI) tol) -> do
--   tol' <- case tol of
--     [] -> do
--       newKids <-
--         map (X.cdData . head . X.onlyText . X.elContent . fromJust .
--           X.findChild (X.unqual "name")) .
--         X.findChildren (X.unqual "taxon") .
--         fromJust . X.findChild (X.unqual "child_taxa") .
--         head . X.onlyElems . X.elContent . (!! 1) . X.onlyElems .
--         X.parseXML <$> openURL (
--         "http://www.catalogueoflife.org/col/webservice?name=" ++ urlize s ++
--         "&response=full")
--       return [Node (x, Nothing) [] | x <- newKids]
--     _ -> return tol
--   Node (s, mI) <$> fillNodes parent toLevel tol'
--
-- fillNodes :: Taxon -> Int -> Tol -> [(IO a, a -> Tol -> Tol)]
-- fillNodes parent toLevel subTrees = do
--   r <- forM subTrees $ \ (Node (s, mI) tol) -> do
--     let parent' = parent ++ [s]
--     mI' <- case mI of
--       Nothing -> Just <$> taxonToSpeciesCount parent'
--       _ -> return mI
--     return $ Node (s, mI') tol
--   growTol parent (toLevel - 1) r
