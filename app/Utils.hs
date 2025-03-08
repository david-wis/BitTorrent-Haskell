{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils(
    segmentBytestring, 
    Address (Address),
    PeerId,
    Hash
) where

import Data.ByteString.Char8 (ByteString, uncons, unsnoc, cons, snoc)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB

data Address = Address String String

type Hash = ByteString

type PeerId = ByteString

instance Show Address where
    show (Address ip port) = ip ++ ":" ++ port


segmentBytestring :: ByteString  -> Int -> [ByteString]
segmentBytestring (B.uncons -> Nothing) n = []
segmentBytestring bs n = B.take n bs : segmentBytestring (B.drop n bs) n