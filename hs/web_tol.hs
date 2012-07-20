#include <h>

import Network.HTTP
import qualified Data.Serialize as Ser

type TOL = Forest (String, Maybe Int)

type Taxon = [String]

kingdoms = [
  "Animalia", 
  "Plantae",
  "Fungi",
  "Protozoa",
  "Bacteria",
  "Chromista",
  "Viruses",
  "Archaea"]

taxonLevels = ["kingdom", "phylum", "class", "order", "family", "genus"]

childrenOfTaxon :: Taxon -> [String]
childrenOfTaxon [] =
  ["Animalia"]
childrenOfTaxon ["Animalia"] = ["Acanthocephala"]
childrenOfTaxon _ = []

openURL x = getResponseBody =<< simpleHTTP (getRequest x)

taxonToSearchUrl :: Taxon -> String
taxonToSearchUrl taxon =
  "http://www.catalogueoflife.org/col/browse/classification/" ++
  intercalate "/" (zipWith (\ n t -> n ++ "/" ++ t) taxonLevels taxon) ++
  "/match/1"

taxonToSpeciesCount :: [String] -> IO Int
taxonToSpeciesCount [] = return 1315904
taxonToSpeciesCount taxon = do
  ls <- lines <$> openURL (taxonToSearchUrl taxon)
  let 
    line = head . drop 1 $ dropWhile (not . (isPrefixOf "Records found:")) ls
    word = head . drop 1 $ words line
    num = read . filter (/= ',') $ drop 1 word
  return num

{-
doTaxon :: Taxon -> String -> IO (Tree (Taxon, Int))
doTaxon parent taxon = do
  count <- taxonToSpeciesCount taxon
  rec <- forM (childrenOfTaxon taxon) $ \ c -> do
    let
      taxon' = taxon ++ [c]
    doTaxon taxon'
  return $ Node (taxon, count) rec
-}

fillTOL :: Taxon -> Int -> TOL -> IO TOL
fillTOL _ 0 tol = return tol
fillTOL parent toLevel subTrees = forM subTrees $ \ (Node (s, mI) tol) -> do
  mI' <- case mI of
    Nothing -> Just <$> taxonToSpeciesCount (parent ++ [s])
    _ -> return mI
  tol' <- case tol of
    [] -> 
      "http://www.catalogueoflife.org/col/webservice?name=" ++
      ? ++ "&response=full"
    _ -> 
  return $ Node (s, mI') tol'

main = do
  --let startTree = [Node (k, Nothing) [] | k <- kingdoms]
  --print (startTree :: TOL)
  --BS.writeFile "tol" $ Ser.encode tree'
  startTree <- fromRight . Ser.decode <$> BS.readFile "tol"
  tree' <- fillTOL [] 1 startTree
  print tree'
  --BS.writeFile "tol" $ Ser.encode tree'
