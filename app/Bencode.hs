{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bencode (
    BencodedElem (BencodedDict),
    parseBencode,
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
data BencodedElem = BencodedDict [(ByteString, BencodedElem)] 
                    | BencodedArray [BencodedElem] 
                    | BencodedString ByteString 
                    | BencodedInt Int

instance Show BencodedElem where
    show (BencodedArray elems) = show elems
    show (BencodedString s) = show s
    show (BencodedInt i) = show i
    show (BencodedDict elems) = "{" ++ intercalate ", " (map (\(k, v) -> show k ++ ": " ++ show v) elems) ++ "}"


parseBencodedString :: ByteString -> Maybe (BencodedElem, ByteString)
parseBencodedString s = do 
                            pos <- B.elemIndex ':' s 
                            let (sLen, sRemainingRaw) = B.splitAt pos s
                            (len, _) <- B.readInt sLen 
                            let sRemaining = B.tail sRemainingRaw -- Remove ':'
                                (sExtracted, sReturn) = B.splitAt len sRemaining
                            return (BencodedString sExtracted, sReturn)




parseBencodedInt :: ByteString -> Maybe (BencodedElem, ByteString)
parseBencodedInt (uncons -> Just ('i', sNum)) =  do
                                                    tuple <- B.readInt sNum
                                                    (n, sReturn) <- removeE tuple
                                                    return (BencodedInt n, sReturn)
                                              where removeE t@(n, uncons -> Just ('e', sReturn)) = Just (n, sReturn)
                                                    removeE _ = Nothing
parseBencodedInt _ = Nothing

parseBencodedListRecursive :: ByteString -> Maybe ([BencodedElem], ByteString)
parseBencodedListRecursive (uncons -> Just ('e', sReturn)) = Just ([], sReturn)
parseBencodedListRecursive sList = do (elem, sRemaining) <- parseBencodedValue sList
                                      (elems, sReturn) <- parseBencodedListRecursive sRemaining
                                      return (elem:elems, sReturn)

parseBencodedList :: ByteString -> Maybe (BencodedElem, ByteString)
parseBencodedList (uncons -> Just ('l', sList)) = do 
                                                      (elems, sRemaining) <- parseBencodedListRecursive sList
                                                      return (BencodedArray elems, sRemaining)
parseBencodedList _ = Nothing -- Should never happen


-- Nothing if length is odd or any key is not string
groupPairs :: [BencodedElem] -> Maybe [(ByteString, BencodedElem)]
groupPairs [] = Just []
groupPairs ((BencodedString key):e2:es) = do xs <- groupPairs es
                                             return ((key,e2) : xs)
groupPairs _ = Nothing

parseBencodedDict :: ByteString -> Maybe (BencodedElem, ByteString)
parseBencodedDict (uncons -> Just ('d', sList)) = do 
                                                      (elems, sRemaining) <- parseBencodedListRecursive sList
                                                      groupedElems <- groupPairs elems
                                                      return (BencodedDict groupedElems, sRemaining)
parseBencodedDict _ = Nothing


parseBencodedValue :: ByteString -> Maybe (BencodedElem, ByteString)
-- The equivalent version with native haskell strings (instead of bytestrings) would be:
-- parseBencodedValue cs@(c:_) = ...
parseBencodedValue cs@(uncons -> Just (c, _)) = case c of
                                                    'l' -> parseBencodedList cs
                                                    'd' -> parseBencodedDict cs
                                                    'i' -> parseBencodedInt cs
                                                    _ -> if isDigit c then parseBencodedString cs
                                                         else Nothing
parseBencodedValue _ = Nothing

parseBencode :: ByteString -> Maybe BencodedElem
parseBencode = fmap fst . parseBencodedValue


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