{-# LANGUAGE OverloadedStrings #-}

module Disp where

import Data.List
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

padr :: Char -> Int -> BS.ByteString -> BS.ByteString
padr c l s = s <> BSC.replicate (l - BS.length s) c

padl :: Char -> Int -> BS.ByteString -> BS.ByteString
padl c l s = BSC.replicate (l - BS.length s) c <> s

intB :: Int -> BS.ByteString
intB = BSC.pack . show

renderCols :: [[Either BS.ByteString BS.ByteString]] -> [BS.ByteString]
renderCols =
    map (BS.intercalate " ") .
    transpose .
    map (\col ->
        map (formatter (maximum $ map (either BS.length BS.length) col))
        col) .
    transpose
  where
    formatter l (Left s) = padr ' ' l s
    formatter l (Right s) = padl ' ' l s
