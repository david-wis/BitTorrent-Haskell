{-# LANGUAGE BlockArguments #-}
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
import Control.Concurrent.STM (TQueue, TVar, newTQueue, newTVar, readTVarIO, readTVar, writeTQueue, readTQueue)
import Control.Monad.STM ( atomically, check )
import Control.Concurrent.Async ( mapConcurrently_, concurrently_ )
import qualified Data.ByteString as BS
import System.Entropy (getEntropy)
import System.Directory (removeFile, doesDirectoryExist, makeAbsolute)
import System.FilePath ((</>))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Monad

import qualified Tracker as T
import Bencode ( parseBencode, BencodedElem(BencodedDict), bReadString, bReadInt, bencodeToByteString, bencodeGetValue)
import Utils ( segmentByteString, PieceIndex )
import Peer (connectToPeer)
import Torrent (getTorrentFile, torrentFileToHexHash, announce, infoHash, pieces, info, fileSize, pieceLength, getPieceQuantity, name)
import Worker (downloaderWorker, joinerWorker)
import Args (ArgsInfo, loadArgs, inputPath, outputPath, peerCount, threadsPerPeer)

import Control.Concurrent (forkIO, threadDelay)

initSharedState :: PieceIndex -> IO (TQueue (Maybe PieceIndex), TVar Int)
initSharedState pieceQty = atomically $ do
    q <- newTQueue
    mapM_ (writeTQueue q . Just) [0..pieceQty-1]
    countVar <- newTVar pieceQty
    return (q, countVar)

startProgressReporter :: TVar Int -> Int -> IO ()
startProgressReporter piecesLeft totalPieces = void (forkIO $ go totalPieces)
  where
    go r = do  threadDelay 3000000 -- 3 seconds
               r' <- readTVarIO piecesLeft
               when (r > r') (putStrLn $ "Downloaded pieces: " ++ show (totalPieces - r') ++ "/" ++ show totalPieces)
               when (r' > 0) (go r')


run :: ArgsInfo -> IO ()
run args = do
            handle <- openFile (inputPath args) ReadMode
            absOutPath <- makeAbsolute $ outputPath args
            pathExists <- doesDirectoryExist absOutPath
            unless pathExists $ do
                putStrLn $ "Output path does not exist: " ++ absOutPath
                exitWith (ExitFailure 1)

            contents <- B.hGetContents handle
            selfPid  <- getEntropy 20
            case getTorrentFile =<< parseBencode contents of
                Nothing -> putStrLn "Invalid torrent file"
                Just tf -> do
                    -- putStrLn $ "Tracker URL: " ++ announce tf
                    -- putStrLn $ "Info Hash: " ++ torrentFileToHexHash tf
                    putStrLn $ "File Size: " ++ show (fileSize (info tf) `div` (1024*1024)) ++ " MiB"
                    putStrLn $ "Piece Size: " ++ show (pieceLength (info tf) `div` 1024) ++ " KiB"
                    putStrLn $ "Pieces: " ++ show (getPieceQuantity tf)
                    putStrLn $ "Torrent Name: " ++ name (info tf)

                    --  putStrLn $ "Piece Hashes: " ++ concatMap (('\n' : ) . B.unpack . Base16.encode) (segmentByteString (pieces $ info tf) 20)
                    let outputFilename = absOutPath </> (name . info) tf
                    putStrLn $ "Output file: " ++ outputFilename


                    let queryParams = T.TrackerQueryParams { T.infoHash = infoHash tf, T.peerId = selfPid, T.port = 6881, T.uploaded = 0, T.downloaded = 0, T.left = fileSize $ info tf,  T.compact = 1 }
                    trackerInfo <- T.getPeers (announce tf) queryParams

                    let piecesQty = getPieceQuantity tf
                    (queue, piecesLeft) <- initSharedState piecesQty
                    let peers = case peerCount args of
                                     Nothing -> T.peers trackerInfo
                                     (Just n) -> take n $ T.peers trackerInfo
                    putStrLn $ "Peers: " ++ show (length peers)

                    startProgressReporter piecesLeft piecesQty

                    concurrently_ (joinerWorker piecesLeft piecesQty outputFilename)
                                  (mapConcurrently_ (downloaderWorker queue piecesLeft outputFilename (threadsPerPeer args) tf selfPid) peers)
                    endPiecesLeft <- readTVarIO piecesLeft
                    when (endPiecesLeft /= 0) $ do
                        putStrLn $ "Error: Ended with " ++ show endPiecesLeft ++ " pieces!"
                        exitWith (ExitFailure 1)
                    

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    args <- getArgs
    let maybeArgs = loadArgs args
    maybe (putStrLn "Usage: stack run <input_path> <output_path> <threads> <peer_limit>" >> exitWith (ExitFailure 1)) run maybeArgs


