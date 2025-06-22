{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.List (intercalate, find)
import Data.ByteString.Char8 (ByteString, uncons, unsnoc, cons, snoc)
import Data.Char (isDigit)
import System.Environment
import System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import System.IO (hSetBuffering, stdout, stderr,  BufferMode (NoBuffering), IOMode (ReadMode), openFile)
import qualified Data.ByteString.Base16 as Base16
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified Tracker as T
import Bencode ( parseBencodedValue, BencodedElem(BencodedDict), bReadString, bReadInt, bencodeToByteString, bencodeGetValue)
import Utils ( segmentByteString )
import Peer (connectToPeer)
import System.Entropy (getEntropy)
import Torrent (getTorrentFile, torrentFileToHexHash, announce, infoHash, pieces, info, fileSize, pieceLength)
import qualified Data.ByteString as BS

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
            selfPid  <- getEntropy 20
            let outputFilename = args !! 2
            case getTorrentFile $ fst $ parseBencodedValue contents of
                Just tf -> do
                    putStrLn $ "Tracker URL: " ++ announce tf
                    putStrLn $ "Length: " ++ show (fileSize $ info tf)
                    putStrLn $ "Info Hash: " ++ torrentFileToHexHash tf
                    putStrLn $ "Piece Length: " ++ show (pieceLength $ info tf)
                    putStrLn $ "Piece Hashes: " ++ concatMap (('\n' : ) . B.unpack . Base16.encode) (segmentByteString (pieces $ info tf) 20)
                    let queryParams = T.TrackerQueryParams { T.infoHash = infoHash tf, T.peerId = selfPid, T.port = 6881, T.uploaded = 0, T.downloaded = 0, T.left = fileSize $ info tf,  T.compact = 1 }
                    trackerInfo <- T.getPeers (announce tf) queryParams
                    putStrLn $  "First Address: " ++ show (head $ T.peers trackerInfo)
                    fileBS <- connectToPeer (T.peers trackerInfo !! 2) tf selfPid
                    BS.writeFile outputFilename fileBS
                Nothing -> putStrLn "Invalid torrent file"
        _ -> do putStrLn "Invalid command"
