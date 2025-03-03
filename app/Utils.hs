{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Utils(
    segmentBytestring
) where

import Data.ByteString.Char8 (ByteString, uncons, unsnoc, cons, snoc)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB

segmentBytestring :: ByteString  -> Int -> [ByteString]
segmentBytestring (B.uncons -> Nothing) n = []
segmentBytestring bs n = B.take n bs : segmentBytestring (B.drop n bs) n