{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils(
    segmentBytestring, 
    readBytesAsInt,
    Address (Address),
    PeerId,
    Hash
) where

import Data.ByteString.Char8 (ByteString, uncons, unsnoc, cons, snoc)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Bits (shiftL, (.|.))

data Address = Address String String

type Hash = ByteString

type PeerId = ByteString

instance Show Address where
    show (Address ip port) = ip ++ ":" ++ port


segmentBytestring :: ByteString  -> Int -> [ByteString]
segmentBytestring (B.uncons -> Nothing) n = []
segmentBytestring bs n = B.take n bs : segmentBytestring (B.drop n bs) n

readBytesAsInt :: ByteString -> Int -> (Int, ByteString)
readBytesAsInt bs n = let (num, remainder) = B.splitAt n bs
                      in (B.foldl' (\l r -> l `shiftL` 8 + fromEnum r) 0 num, remainder)
