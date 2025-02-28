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
import Bencode ( parseBencodedValue, BencodedElem(BencodedDict), bReadString, bReadInt, bencodeToByteString)
import Crypto.Hash.SHA1 ( hash, finalize )
import qualified Crypto.Hash as H
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

data TorrentInfo = TorrentInfo {
    len :: Int,
    name :: String,
    pieceLength :: Int,
    pieces :: String
}

data TorrentFile = TorrentFile {
    announce :: String,
    info :: TorrentInfo,
    infoHash :: ByteString
} 

instance Show TorrentInfo where
    show (TorrentInfo len name pieceLength pieces) = "TorrentInfo { len = " ++ show len ++ ", name = " ++ show name ++ ", pieceLength = " ++ show pieceLength ++ ", pieces = " ++ show pieces ++ " }"


instance Show TorrentFile where
    show (TorrentFile announce info infoHash) = "TorrentFile { announce = " ++ show announce ++ ", info = " ++ show info ++ ", infoHash = " ++ B.unpack infoHash ++ " }"


getTorrentInfo :: BencodedElem -> Maybe TorrentInfo
getTorrentInfo (BencodedDict kvs) = do
                                      (_, len) <- find ((== "length") . fst) kvs
                                      intLen <- bReadInt len
                                      (_, name) <- find ((== "name") . fst) kvs 
                                      strName <- bReadString name
                                      (_, pieceLength) <- find ((== "piece length") . fst) kvs
                                      intPieceLength <- bReadInt pieceLength
                                      (_, pieces) <- find ((== "pieces") . fst) kvs 
                                      strPieces <- bReadString pieces
                                      return TorrentInfo { len = intLen, name = strName, pieceLength = intPieceLength, pieces = strPieces}


hashInfo :: BencodedElem -> ByteString
hashInfo b = hash $ bencodeToByteString b

getTorrentFile :: BencodedElem -> Maybe TorrentFile
getTorrentFile (BencodedDict kvs) = do 
                                      (_, announce) <- find ((== "announce") . fst) kvs 
                                      strAnnounce <- bReadString announce
                                      (_, info) <- find ((== "info") . fst) kvs
                                      tiInfo <- getTorrentInfo info
                                      return TorrentFile { announce = strAnnounce, info = tiInfo, infoHash = hashInfo info }




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
                decodedValue = fst $ parseBencodedValue $ B.pack encodedValue
            in putStrLn $ show decodedValue
        "info" -> do
            handle <- openFile (args !! 1) ReadMode
            contents <- B.hGetContents handle
            case (getTorrentFile $ fst $ parseBencodedValue contents) of
                Just tf -> do
                    putStrLn $ "Tracker URL: " ++ announce tf
                    putStrLn $ "Length: " ++ (show $ len $ info tf)
                    putStrLn $ "Info Hash: " ++ (T.unpack $ decodeUtf8 $ Base16.encode $ encodeUtf8 $ T.pack $ B.unpack $ infoHash tf)
                    putStrLn $ show tf
                Nothing -> putStrLn "Invalid torrent file"
        "test" -> do
            let (bencoded,_) = parseBencodedValue $ B.pack (args !! 1)
            let hashed = (T.unpack $ decodeUtf8 $ Base16.encode $ encodeUtf8 $ T.pack $ B.unpack $ hashInfo bencoded)
            putStrLn $ show bencoded
            putStrLn $ show $ B.unpack $ bencodeToByteString bencoded
            putStrLn $ show hashed
        _ -> putStrLn $ "Unknown command: " ++ command
