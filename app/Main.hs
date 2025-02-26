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

decodeBencodedString :: ByteString -> ByteString
decodeBencodedString s = case B.elemIndex ':' s of 
                           Just pos -> let (sLen, sExtractedRaw) = B.splitAt pos s
                                           Just (len, _) = B.readInt sLen -- TODO: Add validations
                                           sExtracted = B.tail sExtractedRaw -- Remove ':'
                                       in if len == B.length sExtracted 
                                          then  snoc (cons '"' sExtracted) '"' -- TODO: Check if quotes should be removed
                                          else error "Invalid string length"
                           Nothing -> error "Invalid string format"

decodeBencodedInt :: ByteString -> ByteString
decodeBencodedInt (uncons -> Just ('i', unsnoc -> Just (sNum, 'e'))) = sNum
decodeBencodedInt _ = error "Invalid Int format"



decodeBencodedValue :: ByteString -> ByteString
-- The equivalent version with native haskell strings (instead of bytestrings) would be:
-- decodeBencodedValue cs@(c:_) = ...
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
            let decodedValue = decodeBencodedValue $ B.pack encodedValue
            let jsonValue = B.unpack decodedValue
            putStrLn jsonValue
        _ -> putStrLn $ "Unknown command: " ++ command
