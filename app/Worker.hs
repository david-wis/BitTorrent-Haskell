{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Worker (
    worker
) where

import Data.Maybe (isJust)
import Control.Monad (when)
import Control.Concurrent.STM (TQueue, TVar, readTQueue, writeTQueue, readTVarIO, modifyTVar)
import Control.Concurrent.Async ( replicateConcurrently_ )
import Control.Monad.STM ( atomically )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Network.Simple.TCP (Socket)
import Args (ArgsInfo(ArgsInfo), outputPath, threadsPerPeer)
import qualified Data.ByteString.Base16 as B16 -- TODO: Use with qualified


import Utils (Address, PeerId, PieceIndex, BitField, Path, bitFieldContains)
import Torrent (TorrentFile,  infoHash, pieceLength, info, pieces, fileSize, getPieceQuantity)
import Peer (connectToPeer, downloadPiece)

loopWorker :: TQueue (Maybe PieceIndex) -> TVar Int -> Path -> TorrentFile -> BitField -> Socket -> IO ()
loopWorker queue piecesLeft outputFilename torrentFile bitfield sock =
    -- TODO: Inspect bitField to determine which pieces are available
    do
        let totalSize = fileSize $ info torrentFile
        let pieceSize = pieceLength $ info torrentFile
        let piecesQty = getPieceQuantity torrentFile
        let lastPieceSize = totalSize `mod` pieceSize
        let getPieceSize idx = if idx /= piecesQty-1 || lastPieceSize == 0 then pieceSize else lastPieceSize
        let getPieceHash idx = B.take 20 $ B.drop (20 * idx) $ pieces $ info torrentFile

        remaining <- readTVarIO piecesLeft
        if remaining > 0
        then do
            maybePieceIndex <- atomically $ readTQueue queue
            maybe (return ()) (\pieceIndex ->
                do
                    -- putStrLn $ "Bitfield: " ++ (B.unpack $ B16.encode bitfield)
                    if (bitFieldContains bitfield pieceIndex) -- Check if the peer has the piece
                    then do
                            pieceBS <- downloadPiece sock (getPieceSize pieceIndex) pieceIndex (getPieceHash pieceIndex)
                            BS.writeFile (outputFilename ++ show pieceIndex ++ ".part") pieceBS
                            atomically $ modifyTVar piecesLeft (\x -> x-1)
                    else do
                        putStrLn $ "@peer does not have piece " ++ show pieceIndex
                        atomically $ writeTQueue queue maybePieceIndex
                    loopWorker queue piecesLeft outputFilename torrentFile bitfield sock
                ) maybePieceIndex

        else atomically $ writeTQueue queue Nothing


worker :: TQueue (Maybe PieceIndex) -> TVar Int -> ArgsInfo -> TorrentFile -> PeerId -> Address -> IO ()
worker queue piecesLeft ArgsInfo{outputPath=outPath, threadsPerPeer=tPerPeer} torrentFile peerId addr =
    do
        success <- connectToPeer addr torrentFile peerId (\_ _ -> return ())
        -- connectToPeer addr torrentFile peerId (loopWorker queue piecesLeft outputFilename torrentFile)
        when success $ replicateConcurrently_ tPerPeer $ do
            connectToPeer addr torrentFile peerId (loopWorker queue piecesLeft outPath torrentFile)
            return ()
            -- putStrLn "Failed to connect to peer"
        -- disconnectFromPeer sock
        return ()
