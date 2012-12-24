{-# LANGUAGE DeriveGeneric #-}

module Rank where

import qualified Data.Serialize as Ser
import Data.Serialize.Derive
import GHC.Generics

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
    deriving (Enum, Eq, Generic, Ord, Show, Read)

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

instance Ser.Serialize Rank where
    get = deriveGet
    put = derivePut
