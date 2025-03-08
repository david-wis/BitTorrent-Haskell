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
import Crypto.Hash.SHA1 ( hash )
import qualified Data.ByteString.Base16 as Base16
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Tracker as T
import Bencode ( parseBencodedValue, BencodedElem(BencodedDict), bReadString, bReadInt, bencodeToByteString, bencodeGetValue)
import Utils ( segmentBytestring )
import Peer (handshake)
import System.Entropy (getEntropy)

data TorrentInfo = TorrentInfo {
    len :: Int,
    name :: String,
    pieceLength :: Int,
    pieces :: ByteString
}

data TorrentFile = TorrentFile {
    announce :: String,
    info :: TorrentInfo,
    infoHash :: ByteString
}

instance Show TorrentInfo where
    show (TorrentInfo len name pieceLength pieces) = "TorrentInfo { len = " ++ show len ++ ", name = " ++ show name ++ ", pieceLength = " ++ show pieceLength ++ ", pieces = " ++ (B.unpack $ Base16.encode $ pieces) ++ " }"


instance Show TorrentFile where
    show tf@(TorrentFile announce info infoHash) = "TorrentFile { announce = " ++ show announce ++ ", info = " ++ show info ++ ", infoHash = " ++  torrentFileToHexHash tf ++ "}"


getTorrentInfo :: BencodedElem -> Maybe TorrentInfo
getTorrentInfo bed@(BencodedDict _) = do
                                      len <- bencodeGetValue bed "length"
                                      intLen <- bReadInt len
                                      name <- bencodeGetValue bed "name"
                                      strName <- bReadString name
                                      pieceLength <- bencodeGetValue bed "piece length"
                                      intPieceLength <- bReadInt pieceLength
                                      pieces <- bencodeGetValue bed "pieces"
                                      strPieces <- bReadString pieces
                                      return TorrentInfo { len = intLen, name = B.unpack strName, pieceLength = intPieceLength, pieces = strPieces}
getTorrentInfo _ = error "Bencode elem is not a dictionary"


hashInfo :: BencodedElem -> ByteString
hashInfo b = hash $ bencodeToByteString b

getTorrentFile :: BencodedElem -> Maybe TorrentFile
getTorrentFile bed@(BencodedDict _) = do
                                      announce <- bencodeGetValue bed "announce"
                                      strAnnounce <- bReadString announce
                                      info <- bencodeGetValue bed "info"
                                      tiInfo <- getTorrentInfo info
                                      return TorrentFile { announce = B.unpack strAnnounce, info = tiInfo, infoHash = hashInfo info }
getTorrentFile _ = error "Bencode elem is not a dictionary"

torrentFileToHexHash :: TorrentFile -> String
torrentFileToHexHash tf = B.unpack $ Base16.encode $ infoHash tf

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
            in print decodedValue
        "info" -> do
            handle <- openFile (args !! 1) ReadMode
            contents <- B.hGetContents handle
            clientPeerId  <- getEntropy 20
            case getTorrentFile $ fst $ parseBencodedValue contents of
                Just tf -> do
                    putStrLn $ "Tracker URL: " ++ announce tf
                    putStrLn $ "Length: " ++ show (len $ info tf)
                    putStrLn $ "Info Hash: " ++ torrentFileToHexHash tf
                    putStrLn $ "Piece Length: " ++ show (pieceLength $ info tf)
                    putStrLn $ "Piece Hashes: " ++ concatMap (('\n' : ) . B.unpack . Base16.encode) (segmentBytestring (pieces $ info tf) 20)
                    -- TODO: Randomize peerId
                    let queryParams = T.TrackerQueryParams { T.infoHash = infoHash tf, T.peerId = clientPeerId, T.port = 6881, T.uploaded = 0, T.downloaded = 0, T.left = len $ info tf,  T.compact = 1 }
                    trackerInfo <- T.getPeers (announce tf) queryParams
                    rsp <- handshake (head $ T.peers trackerInfo) (infoHash tf) clientPeerId
                    putStrLn $ "Peer id: " ++ B.unpack (Base16.encode rsp)
                    -- print tf
                Nothing -> putStrLn "Invalid torrent file"
        _ -> do putStrLn "Invalid command"
