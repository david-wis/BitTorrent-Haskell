{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}    

module Bencode (
    BencodedElem (BencodedDict), 
    parseBencodedValue,
    bReadInt,
    bReadString,
    bencodeToByteString,
    bencodeGetValue
) where

import Data.Aeson
import Data.List (intercalate, find)
import Data.ByteString.Char8 (ByteString, uncons, unsnoc, cons, snoc)
import Data.Char (isDigit)
import System.Environment
import System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import System.IO (hSetBuffering, stdout, stderr,  BufferMode (NoBuffering))


-- Warning: dict keys should always be Strings
data BencodedElem = BencodedDict [(ByteString, BencodedElem)] | BencodedArray [BencodedElem] | BencodedString ByteString | BencodedInt Int

-- Make BencodedElem showable
instance Show BencodedElem where
    show (BencodedArray elems) = show elems
    show (BencodedString s) = show s
    show (BencodedInt i) = show i
    show (BencodedDict elems) = "{" ++ (intercalate ", " $ map (\(k, v) -> show k ++ ": " ++ show v) elems) ++ "}"


parseBencodedString :: ByteString -> (BencodedElem, ByteString)
parseBencodedString s = case B.elemIndex ':' s of 
                           Just pos -> let (sLen, sRemainingRaw) = B.splitAt pos s
                                           Just (len, _) = B.readInt sLen -- TODO: Add validations
                                           sRemaining = B.tail sRemainingRaw -- Remove ':'
                                           (sExtracted, sReturn) = B.splitAt len sRemaining
                                       in (BencodedString sExtracted, sReturn) -- TODO: Check what happens if the length is wrong
                           Nothing -> error "Invalid string format"

parseBencodedInt :: ByteString -> (BencodedElem, ByteString)
parseBencodedInt (uncons -> Just ('i', sNum)) = case B.readInt sNum of
                                                      Just (n, uncons -> Just ('e', sReturn)) -> (BencodedInt n, sReturn)
                                                      Nothing -> error "Invalid Int format" 
parseBencodedInt _ = error "Invalid Int format"

parseBencodedListRecursive :: ByteString -> ([BencodedElem], ByteString)
parseBencodedListRecursive (uncons -> Just ('e', sReturn)) = ([], sReturn)
parseBencodedListRecursive sList = let (elem, sRemaining) = parseBencodedValue sList
                                       (elems, sReturn) = parseBencodedListRecursive sRemaining
                                    in (elem:elems, sReturn)

parseBencodedList :: ByteString -> (BencodedElem, ByteString)
parseBencodedList (uncons -> Just ('l', sList)) = let (elems, sRemaining) = parseBencodedListRecursive sList
                                                   in (BencodedArray elems, sRemaining)


parseBencodedDict :: ByteString -> (BencodedElem, ByteString)
parseBencodedDict (uncons -> Just ('d', sList)) = let (elems, sRemaining) = parseBencodedListRecursive sList
                                                      groupedElems = groupPairs elems
                                                   in (BencodedDict groupedElems, sRemaining)
                                                 where groupPairs [] = []
                                                       groupPairs ((BencodedString key):e2:es) = (key,e2) : groupPairs es
                                                       groupPairs _ = error "Wrong dict parity or Key is not String"


parseBencodedValue :: ByteString -> (BencodedElem, ByteString)
-- The equivalent version with native haskell strings (instead of bytestrings) would be:
-- parseBencodedValue cs@(c:_) = ...
parseBencodedValue cs@(uncons -> Just (c, _)) = case c of 
                                                    'l' -> parseBencodedList cs
                                                    'd' -> parseBencodedDict cs
                                                    'i' -> parseBencodedInt cs
                                                    _ -> if isDigit c then parseBencodedString cs
                                                         else error "TODO"

-- TODO: See if we should use an Exception Monad instead of Maybe

bReadString :: BencodedElem -> Maybe ByteString
bReadString (BencodedString s) = Just s
bReadString _ = Nothing

bReadInt :: BencodedElem -> Maybe Int
bReadInt (BencodedInt i) = Just i
bReadInt _ =  Nothing

bencodeString :: ByteString -> ByteString
bencodeString s = B.snoc (B.pack $ show $ B.length s) ':' `B.append` s

bencodeToByteString :: BencodedElem -> ByteString
bencodeToByteString (BencodedInt i) = B.pack $ "i" ++ show i ++ "e"
bencodeToByteString (BencodedString s) = bencodeString s
bencodeToByteString (BencodedArray bes) = ('l' `cons` bytestringArray) `snoc` 'e'
                                       where bytestringArray = foldr (B.append . bencodeToByteString) B.empty bes
bencodeToByteString (BencodedDict kvs) = ('d' `cons` bytestringDict) `snoc` 'e'
                                      where bytestringDict = foldr (\(k,v) bs -> B.append (bencodeString k) (B.append (bencodeToByteString v) bs)) B.empty kvs

bencodeGetValue :: BencodedElem -> ByteString -> Maybe BencodedElem
bencodeGetValue (BencodedDict kvs) k = fmap snd (find ((== k) . fst) kvs)
bencodeGetValue _ _ = Nothing