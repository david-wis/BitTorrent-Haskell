{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}    
import Data.Aeson
import Data.List (intercalate, find)
import Data.ByteString.Char8 (ByteString, uncons, unsnoc, cons, snoc)
import Data.Char (isDigit)
import System.Environment
import System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import System.IO (hSetBuffering, stdout, stderr,  BufferMode (NoBuffering), IOMode (ReadMode), openFile)
import Parser ( decodeBencodedValue, BencodeElem(BencodeDict), bReadString, bReadInt )

data TorrentInfo = TorrentInfo {
    len :: Int,
    name :: String,
    pieceLength :: Int,
    pieces :: String
}

data TorrentFile = TorrentFile {
    announce :: String,
    info :: TorrentInfo
} 

instance Show TorrentInfo where
    show (TorrentInfo len name pieceLength pieces) = "TorrentInfo { len = " ++ show len ++ ", name = " ++ show name ++ ", pieceLength = " ++ show pieceLength ++ ", pieces = " ++ show pieces ++ " }"


instance Show TorrentFile where
    show (TorrentFile announce info) = "TorrentFile { announce = " ++ show announce ++ ", info = " ++ show info ++ " }"


getTorrentInfo :: BencodeElem -> Maybe TorrentInfo
getTorrentInfo (BencodeDict kvs) = do
                                      (_, len) <- find ((== "length") . fst) kvs
                                      intLen <- bReadInt len
                                      (_, name) <- find ((== "name") . fst) kvs 
                                      strName <- bReadString name
                                      (_, pieceLength) <- find ((== "piece length") . fst) kvs
                                      intPieceLength <- bReadInt pieceLength
                                      (_, pieces) <- find ((== "pieces") . fst) kvs 
                                      strPieces <- bReadString pieces
                                      return TorrentInfo { len = intLen, name = strName, pieceLength = intPieceLength, pieces = strPieces}


getTorrentFile :: BencodeElem -> Maybe TorrentFile
getTorrentFile (BencodeDict kvs) = do 
                                      (_, announce) <- find ((== "announce") . fst) kvs 
                                      strAnnounce <- bReadString announce
                                      (_, info) <- find ((== "info") . fst) kvs
                                      tiInfo <- getTorrentInfo info
                                      return TorrentFile { announce = strAnnounce, info = tiInfo }


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
        "decode" ->
            -- You can use print statements as follows for debugging, they'll be visible when running tests.
            -- hPutStrLn stderr "Logs from your program will appear here!"
            -- Uncomment this block to pass stage 1
            let encodedValue = args !! 1
                decodedValue = fst $ decodeBencodedValue $ B.pack encodedValue
            in putStrLn $ show decodedValue
        "info" -> do
            handle <- openFile (args !! 1) ReadMode
            contents <- B.hGetContents handle
            case (getTorrentFile $ fst $ decodeBencodedValue contents) of
                Just tf -> putStrLn $ show tf
                Nothing -> putStrLn "Invalid torrent file"
        _ -> putStrLn $ "Unknown command: " ++ command
