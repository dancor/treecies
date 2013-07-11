{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TolNode where

import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser
import Data.Serialize.Derive
import GHC.Generics

import Rank

data TolNode = TolNode
    { tnId    :: Int
    , tnName  :: BS.ByteString
    , tnRank  :: Rank
    } deriving (Show, Generic)

instance Ser.Serialize TolNode where
    get = deriveGet
    put = derivePut

initialTolNode :: TolNode
initialTolNode = TolNode 0 "<root>" RankAll
