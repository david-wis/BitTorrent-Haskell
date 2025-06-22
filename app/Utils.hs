{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils(
    segmentByteString, 
    readBytesAsInt,
    Address (Address),
    PeerId,
    Hash,
    intToByteString
) where

import Data.ByteString.Char8 (ByteString, uncons, unsnoc, cons, snoc)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Bits (shiftL, (.|.))
import qualified Data.Binary as Bin
import Data.Int (Int32)

data Address = Address String String

type Hash = ByteString

type PeerId = ByteString

instance Show Address where
    show (Address ip port) = ip ++ ":" ++ port

intToByteString :: Int -> ByteString
intToByteString n = LB.toStrict (Bin.encode (fromIntegral n :: Int32))

segmentByteString :: ByteString  -> Int -> [ByteString]
segmentByteString (B.uncons -> Nothing) n = []
segmentByteString bs n = B.take n bs : segmentByteString (B.drop n bs) n

readBytesAsInt :: ByteString -> Int -> (Int, ByteString)
readBytesAsInt bs n = let (num, remainder) = B.splitAt n bs
                      in (B.foldl' (\l r -> l `shiftL` 8 + fromEnum r) 0 num, remainder)
