{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}    
import Data.Aeson
import Data.ByteString.Char8 (ByteString, uncons, unsnoc, cons, snoc)
import Data.Char (isDigit)
import System.Environment
import System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import System.IO (hSetBuffering, stdout, stderr,  BufferMode (NoBuffering))


data BencodeElem = BencodeArray [BencodeElem] | BencodeString String | BencodeInt Int

-- Make BencodeElem showable
instance Show BencodeElem where
    show (BencodeArray elems) = show elems
    show (BencodeString s) = show s
    show (BencodeInt i) = show i


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


decodeBencodedValue :: ByteString -> (BencodeElem, ByteString)
-- The equivalent version with native haskell strings (instead of bytestrings) would be:
-- decodeBencodedValue cs@(c:_) = ...
decodeBencodedValue cs@(uncons -> Just ('l', _)) = decodeBencodedList cs
decodeBencodedValue cs@(uncons -> Just ('i', _)) = decodeBencodedInt cs
decodeBencodedValue cs@(uncons -> Just (c, _)) = if isDigit c then decodeBencodedString cs
                                                 else error "TODO"

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    args <- getArgs
    if length args < 2
        then do 
            putStrLn "Usage: your_bittorrent.sh <command> <args>"
            exitWith (ExitFailure 1)
        else return ()

    let command = args !! 0
    case command of
        "decode" -> do
            -- You can use print statements as follows for debugging, they'll be visible when running tests.
            -- hPutStrLn stderr "Logs from your program will appear here!"
            -- Uncomment this block to pass stage 1
            let encodedValue = args !! 1
            let decodedValue = fst $ decodeBencodedValue $ B.pack encodedValue
            -- let jsonValue = B.unpack decodedValue
            putStrLn $ show decodedValue
        _ -> putStrLn $ "Unknown command: " ++ command
