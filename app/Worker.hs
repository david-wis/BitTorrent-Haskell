{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Worker (
    worker
) where

import Data.Maybe (isJust)
import Control.Monad (when, unless)
import Control.Concurrent.STM (TQueue, TVar, readTQueue, writeTQueue, readTVarIO, modifyTVar)
import Control.Concurrent.Async ( replicateConcurrently_ )
import Control.Monad.STM ( atomically )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Network.Simple.TCP (Socket)
import Args (ArgsInfo(ArgsInfo), outputPath, threadsPerPeer)
import qualified Data.ByteString.Base16 as B16


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
            maybe (return ()) (\pieceIndex ->
                do
                    -- putStrLn $ "Bitfield: " ++ (B.unpack $ B16.encode bitfield)
                    if bitFieldContains bitfield pieceIndex -- Check if the peer has the piece
                    then do
                            pieceBS <- downloadPiece sock (getPieceSize piecesQty pieceSize lastPieceSize pieceIndex) pieceIndex (getPieceHash torrentFile pieceIndex)
                                        `catch` \(e :: SomeException) -> atomically (writeTQueue queue maybePieceIndex) >> throw e

                            BS.writeFile (outputFilename ++ show pieceIndex ++ ".part") pieceBS
                            atomically $ modifyTVar piecesLeft (\x -> x-1)
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


worker :: TQueue (Maybe PieceIndex) -> TVar Int -> Path -> Int -> TorrentFile -> PeerId -> Address -> IO ()
worker queue piecesLeft outputFileNime tPerPeer torrentFile peerId addr =
    do
        success <- connectToPeer addr torrentFile peerId (\_ _ -> return ())
        when success $ replicateConcurrently_ tPerPeer $ retryConnection False
    where retryConnection succeded = unless succeded $ do 
                                                    succeded' <- connectToPeer addr torrentFile peerId (loopWorker queue piecesLeft outputFileNime torrentFile)
                                                    retryConnection succeded'
