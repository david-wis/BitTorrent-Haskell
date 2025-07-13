{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Worker (
    downloaderWorker, joinerWorker
) where

import Data.Maybe (isJust)
import Control.Monad (when, unless)
import Control.Concurrent.STM (TQueue, TVar, readTQueue, writeTQueue, readTVarIO, modifyTVar, readTVar)
import Control.Concurrent.Async ( replicateConcurrently_, mapConcurrently_ )
import Control.Monad.STM ( atomically, check )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Network.Simple.TCP (Socket)
import Args (ArgsInfo(ArgsInfo), outputPath, threadsPerPeer)
import qualified Data.ByteString.Base16 as B16
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (removeFile)


import Utils (Address, PeerId, PieceIndex, BitField, Path, bitFieldContains)
import Torrent (TorrentFile,  infoHash, pieceLength, info, pieces, fileSize, getPieceQuantity)
import Peer (connectToPeer, downloadPiece)
import Control.Exception (throw, catch, SomeException)

getPieceSize :: Int -> Int -> Int -> Int -> Int
getPieceSize piecesQty pieceSize lastPieceSize idx =
    if idx /= piecesQty-1 || lastPieceSize == 0 then pieceSize else lastPieceSize

getPieceHash :: TorrentFile -> Int -> ByteString
getPieceHash torrentFile idx =
    B.take 20 $ B.drop (20 * idx) $ pieces $ info torrentFile

loopWorker :: TQueue (Maybe PieceIndex) -> TVar Int -> Path -> TorrentFile -> BitField -> Socket -> IO ()
loopWorker queue piecesLeft outputFilename torrentFile bitfield sock =
    do
        remaining <- readTVarIO piecesLeft
        if remaining > 0
        then do
            maybePieceIndex <- atomically $ readTQueue queue
            maybe (atomically $ writeTQueue queue Nothing) (\pieceIndex ->
                do
                    -- putStrLn $ "Bitfield: " ++ (B.unpack $ B16.encode bitfield)
                    if bitFieldContains bitfield pieceIndex -- Check if the peer has the piece
                    then do
                            pieceBS <- downloadPiece sock (getPieceSize piecesQty pieceSize lastPieceSize pieceIndex) pieceIndex (getPieceHash torrentFile pieceIndex)
                                        `catch` \(e :: SomeException) -> atomically (writeTQueue queue maybePieceIndex) >> print e >> throw e

                            BS.writeFile (outputFilename ++ show pieceIndex ++ ".part") pieceBS
                            numPiecesLeft <- atomically $ modifyTVar piecesLeft (\x -> x-1) >> readTVar piecesLeft
                            when (numPiecesLeft == 0) do
                                putStrLn "Downloaded last piece"
                                atomically $ writeTQueue queue Nothing
                    else do
                        putStrLn $ "@peer does not have piece " ++ show pieceIndex
                        atomically $ writeTQueue queue maybePieceIndex
                    loopWorker queue piecesLeft outputFilename torrentFile bitfield sock
                ) maybePieceIndex

        else atomically $ writeTQueue queue Nothing
    where 
        totalSize = fileSize $ info torrentFile
        pieceSize = pieceLength $ info torrentFile
        piecesQty = getPieceQuantity torrentFile
        lastPieceSize = totalSize `mod` pieceSize


downloaderWorker :: TQueue (Maybe PieceIndex) -> TVar Int -> Path -> Int -> TorrentFile -> PeerId -> Address -> IO ()
downloaderWorker queue piecesLeft outputFileNime tPerPeer torrentFile peerId addr =
   do
        success <- connectToPeer addr torrentFile peerId (\_ _ -> return ())
        when success $ replicateConcurrently_ tPerPeer $ retryConnection False
    where retryConnection succeded = unless succeded $ do 
                                                    succeded' <- connectToPeer addr torrentFile peerId (loopWorker queue piecesLeft outputFileNime torrentFile)
                                                    retryConnection succeded'


joinerWorker :: TVar Int -> Int -> Path -> IO ()
joinerWorker piecesLeft piecesQty outputFilename = do
                                                    atomically $ do 
                                                        remaining <- readTVar piecesLeft
                                                        check (remaining == 0)
                                                    putStrLn "All pieces downloaded, joining files..."
                                                    joinTime <- getCurrentTime
                                                    mapM_ (\i -> do
                                                        bs <- BS.readFile (outputFilename ++ show i ++ ".part")
                                                        removeFile (outputFilename ++ show i ++ ".part")
                                                        BS.appendFile outputFilename bs) [0..piecesQty-1]
                                                    putStrLn $ "Files joined successfully. File saved as: " ++ outputFilename
                                                    endTime <- getCurrentTime
                                                    putStrLn $ "Write duration: " ++ show (diffUTCTime endTime joinTime)
                                                    return ()