{-# LANGUAGE OverloadedStrings #-}

module Rank where

import qualified Data.ByteString as BS

-- The Catalogue of Life includes "superfamily" for certain groups,
-- such as large beetle orders.
data Rank
    = RankAll
    | Kingdom
    | Phylum
    | Class
    | Order
    | Superfamily
    | Family
    | Genus
    | Species
    deriving (Enum, Eq, Ord, Show, Read)

rankPlural :: Rank -> BS.ByteString
rankPlural RankAll     = error "rankPlural: RankAll"
rankPlural Kingdom     = "Kingdoms"
rankPlural Phylum      = "Phyla"
rankPlural Class       = "Classes"
rankPlural Order       = "Orders"
rankPlural Superfamily = "Superfamilies"
rankPlural Family      = "Families"
rankPlural Genus       = "Genera"
rankPlural Species     = "Species"

rankAbbr :: Rank -> Char
rankAbbr RankAll     = '@'
rankAbbr Kingdom     = 'K'
rankAbbr Phylum      = 'P'
rankAbbr Class       = 'C'
rankAbbr Order       = 'O'
rankAbbr Superfamily = 'A'
rankAbbr Family      = 'F'
rankAbbr Genus       = 'G'
rankAbbr Species     = 'S'

abbrToRank :: Char -> Rank
abbrToRank '@' = RankAll
abbrToRank 'K' = Kingdom
abbrToRank 'P' = Phylum
abbrToRank 'C' = Class
abbrToRank 'O' = Order
abbrToRank 'A' = Superfamily
abbrToRank 'F' = Family
abbrToRank 'G' = Genus
abbrToRank 'S' = Species
abbrToRank c = error $ "abbrToRank: " ++ [c]

mainRanks :: [Rank]
mainRanks =
    [ Kingdom
    , Phylum
    , Class
    , Order
    , Family
    , Genus
    , Species
    ]
