{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Worker (
    worker
) where

import Control.Monad (when)
import Control.Concurrent.STM (TQueue, TVar, readTQueue, writeTQueue, readTVarIO, modifyTVar)
import Control.Concurrent.Async ( replicateConcurrently_ )
import Control.Monad.STM ( atomically )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Network.Simple.TCP (Socket)


import Utils (Address, PeerId, PieceIndex, BitField, Path)
import Torrent (TorrentFile,  infoHash, pieceLength, info, pieces, fileSize, getPieceQuantity)
import Peer (connectToPeer, downloadPiece)


loopWorker :: TQueue PieceIndex -> TVar Int -> Path -> TorrentFile -> BitField -> Socket -> IO ()
loopWorker queue piecesLeft outputFilename torrentFile bitfield sock =
    -- TODO: Inspect bitField to determine which pieces are available
    do
        remaining <- readTVarIO piecesLeft
        if remaining > 0
        then do
            pieceIndex <- atomically $ readTQueue queue
            Control.Monad.when (pieceIndex /= -1) $ do
                let totalSize = fileSize $ info torrentFile
                let pieceSize = pieceLength $ info torrentFile
                let piecesQty = getPieceQuantity torrentFile
                let lastPieceSize = totalSize `mod` pieceSize
                let getPieceSize idx = if idx /= piecesQty-1 || lastPieceSize == 0 then pieceSize else lastPieceSize
                let getPieceHash idx = B.take 20 $ B.drop (20 * idx) $ pieces $ info torrentFile
                pieceBS <- downloadPiece sock (getPieceSize pieceIndex) pieceIndex (getPieceHash pieceIndex)
                BS.writeFile (outputFilename ++ show pieceIndex ++ ".part") pieceBS
                atomically $ modifyTVar piecesLeft (\x -> x-1)
                loopWorker queue piecesLeft outputFilename torrentFile bitfield sock
        else do
            putStrLn "All pieces downloaded!"


worker :: TQueue PieceIndex -> TVar Int -> Path -> TorrentFile -> PeerId -> Address -> IO ()
worker queue piecesLeft outputFilename torrentFile peerId addr =
    do
        success <- connectToPeer addr torrentFile peerId (loopWorker queue piecesLeft outputFilename torrentFile)
        if success 
        then replicateConcurrently_ 5 $ do
            sock <- connectToPeer addr torrentFile peerId (loopWorker queue piecesLeft outputFilename torrentFile)
            connectToPeer addr torrentFile peerId (loopWorker queue piecesLeft outputFilename torrentFile)
            return ()
        else do
            putStrLn "Failed to connect to peer"
        atomically $ writeTQueue queue (-1)
        -- disconnectFromPeer sock
        return ()
