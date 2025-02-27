{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}    

module Parser (
    BencodeElem (BencodeDict), 
    decodeBencodedValue,
    bReadInt,
    bReadString
) where

import Data.Aeson
import Data.List (intercalate)
import Data.ByteString.Char8 (ByteString, uncons, unsnoc, cons, snoc)
import Data.Char (isDigit)
import System.Environment
import System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import System.IO (hSetBuffering, stdout, stderr,  BufferMode (NoBuffering))


-- Warning: dict keys should always be Strings
data BencodeElem = BencodeDict [(String, BencodeElem)] | BencodeArray [BencodeElem] | BencodeString String | BencodeInt Int

-- Make BencodeElem showable
instance Show BencodeElem where
    show (BencodeArray elems) = show elems
    show (BencodeString s) = show s
    show (BencodeInt i) = show i
    show (BencodeDict elems) = "{" ++ (intercalate ", " $ map (\(k, v) -> show k ++ ": " ++ show v) elems) ++ "}"


decodeBencodedString :: ByteString -> (BencodeElem, ByteString)
decodeBencodedString s = case B.elemIndex ':' s of 
                           Just pos -> let (sLen, sRemainingRaw) = B.splitAt pos s
                                           Just (len, _) = B.readInt sLen -- TODO: Add validations
                                           sRemaining = B.tail sRemainingRaw -- Remove ':'
                                           (sExtracted, sReturn) = B.splitAt len sRemaining
                                       in (BencodeString $ B.unpack sExtracted, sReturn) -- TODO: Check what happens if the length is wrong
                           Nothing -> error "Invalid string format"

decodeBencodedInt :: ByteString -> (BencodeElem, ByteString)
decodeBencodedInt (uncons -> Just ('i', sNum)) = case B.readInt sNum of
                                                      Just (n, uncons -> Just ('e', sReturn)) -> (BencodeInt n, sReturn)
                                                      Nothing -> error "Invalid Int format" 
decodeBencodedInt _ = error "Invalid Int format"

decodeBencodedListRecursive :: ByteString -> ([BencodeElem], ByteString)
decodeBencodedListRecursive (uncons -> Just ('e', sReturn)) = ([], sReturn)
decodeBencodedListRecursive sList = let (elem, sRemaining) = decodeBencodedValue sList
                                        (elems, sReturn) = decodeBencodedListRecursive sRemaining
                                    in (elem:elems, sReturn)

decodeBencodedList :: ByteString -> (BencodeElem, ByteString)
decodeBencodedList (uncons -> Just ('l', sList)) = let (elems, sRemaining) = decodeBencodedListRecursive sList
                                                   in (BencodeArray elems, sRemaining)


decodeBencodedDict :: ByteString -> (BencodeElem, ByteString)
decodeBencodedDict (uncons -> Just ('d', sList)) = let (elems, sRemaining) = decodeBencodedListRecursive sList
                                                       groupedElems = groupPairs elems
                                                   in (BencodeDict groupedElems, sRemaining)
                                                 where groupPairs [] = []
                                                       groupPairs ((BencodeString key):e2:es) = (key,e2) : groupPairs es
                                                       groupPairs _ = error "Wrong dict parity or Key is not String"


decodeBencodedValue :: ByteString -> (BencodeElem, ByteString)
-- The equivalent version with native haskell strings (instead of bytestrings) would be:
-- decodeBencodedValue cs@(c:_) = ...
decodeBencodedValue cs@(uncons -> Just (c, _)) = case c of 
                                                    'l' -> decodeBencodedList cs
                                                    'd' -> decodeBencodedDict cs
                                                    'i' -> decodeBencodedInt cs
                                                    _ -> if isDigit c then decodeBencodedString cs
                                                         else error "TODO"

-- TODO: See if we should use an Exception Monad instead of Maybe

bReadString :: BencodeElem -> Maybe String
bReadString (BencodeString s) = Just s
bReadString _ = Nothing

bReadInt :: BencodeElem -> Maybe Int
bReadInt (BencodeInt i) = Just i
bReadInt _ =  Nothing