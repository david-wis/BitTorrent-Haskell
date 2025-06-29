{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils(
    segmentByteString, 
    readBytesAsInt,
    intToByteString,
    bitFieldContains,
    Address (Address),
    PeerId,
    Hash,
    PieceIndex,
    BlockIndex,
    BitField,
    Path
) where

import Data.ByteString.Char8 (ByteString, uncons, unsnoc, cons, snoc)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Bits (shiftL, (.|.), (.&.))
import qualified Data.Binary as Bin
import Control.Concurrent.STM (TQueue, TVar)
import Data.Int (Int32)
import Control.Monad (when)
data Address = Address String String

type Hash = ByteString

type PeerId = ByteString

type PieceIndex = Int

type BlockIndex = Int

type PieceQueue = TQueue PieceIndex

type BitField = ByteString

type Path = String

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

-- | Checks if a bit field contains a specific piece index.
-- | Warning: This function assumes that the bit field is large enough to contain the piece index.
bitFieldContains :: ByteString -> Int -> Bool
bitFieldContains bs n = let byte = bs `B.index` (n `div` 8) 
                            bitPosition = 7 - (n `mod` 8)
                            mask = (1 `shiftL` bitPosition)
                            in (mask .&. fromEnum byte) /= 0

whenJust :: Monad m => Maybe a -> (a -> Bool) -> (a -> m ()) -> m ()
-- whenJust Nothing _ _  = return ()
-- whenJust (Just x) p f = if p x 
--                         then f x
--                         else return ()
whenJust mx p f = maybe (return ()) (\x -> when (p x) (f x)) mx

