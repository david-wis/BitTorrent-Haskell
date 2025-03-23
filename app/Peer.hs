{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Peer (
    connectToPeer
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Char8 (ByteString)
import Data.Word (Word8)
import Data.Int (Int32)
import Utils (Address (Address), PeerId, Hash, readBytesAsInt)
import Network.Simple.TCP
import qualified Data.ByteString.Base16 as B16 -- TODO: Use with qualified
import qualified Data.Binary as Bin

import Torrent (TorrentFile,  infoHash, pieceLength, info, pieces)
import System.IO

type MessageId = Char

unchokeMessageId :: MessageId
unchokeMessageId = '\1'

interestMessageId :: MessageId
interestMessageId = '\2'

bitFieldMessageId :: MessageId
bitFieldMessageId = '\5'

requestMessageId :: MessageId
requestMessageId = '\6'

pieceMessageId :: MessageId
pieceMessageId = '\7'


-- Start Constants
handshakePrefix :: ByteString
handshakePrefix = "\19BitTorrent protocol"

reservedBytes :: ByteString
reservedBytes = "\0\0\0\0\0\0\0\0"

handshakePrefixLength :: Int
handshakePrefixLength = 20

reservedBytesLength :: Int
reservedBytesLength = 8

hashLength :: Int
hashLength = 20

peerIdLength :: Int
peerIdLength = 20

blockSize :: Int
blockSize = 16384 -- 16 KiB
-- End Constants

connectToPeer :: Address -> TorrentFile -> PeerId -> IO ()
connectToPeer (Address ip port) tf selfPid = connect ip port (handlePeerConnection tf selfPid)


handlePeerConnection :: TorrentFile -> PeerId -> (Socket, SockAddr) -> IO ()
handlePeerConnection tf selfPid (sock, addr) = do
                                                    peerId <- handleHandshake (infoHash tf) selfPid sock
                                                    putStrLn $ "Connection successful to peer with pid: " ++ B.unpack (B16.encode peerId)
                                                    handleBitField sock
                                                    sendInterestedMessage sock
                                                    handleUnchoke sock
                                                    let size = pieceLength $ info tf
                                                    print $ "Piece size is: " ++ show size
                                                    let piecesQty = B.length (pieces $ info tf) `div` 20
                                                    pieceList <- sequence [ downloadPiece sock size pieceIdx | pieceIdx <- [0..piecesQty-1]]
                                                    print $ B.concat pieceList
                                                    return ()


handleHandshake :: Hash -> PeerId -> Socket -> IO PeerId
handleHandshake hash selfPid sock = do
                                  let msg = handshakePrefix `B.append` reservedBytes `B.append` hash `B.append` selfPid
                                  send sock msg
                                  maybeRsp <- recv sock (handshakePrefixLength + reservedBytesLength + hashLength + peerIdLength)
                                  case maybeRsp of
                                    Just pid -> let prefix = B.take handshakePrefixLength pid in
                                                return (
                                                  if prefix == handshakePrefix
                                                    then B.drop (handshakePrefixLength + reservedBytesLength + hashLength) pid
                                                    else error "Invalid response" -- TODO: Handle this better
                                                )
                                    Nothing -> error "No response"


readUntilLength :: Socket -> Int -> IO ByteString
readUntilLength sock n = do
                            maybeRsp <- recv sock n
                            case maybeRsp of
                              Just rsp -> let rspLen = B.length rsp
                                              remainingLen = n - rspLen
                                          in if remainingLen > 0 then do
                                                                        remaining <- readUntilLength sock remainingLen
                                                                        return (rsp `B.append` remaining)
                                                             else return rsp
                              Nothing -> error "Failure reading from socket"


readPeerMessage :: Socket -> IO (MessageId, ByteString)
readPeerMessage sock = do
                          maybeRsp <- recv sock 5
                          case maybeRsp of
                            Just bs ->  do
                                          let (msgLen, msgIdBs) = readBytesAsInt bs 4
                                          print $ "Response: " ++ B.unpack (bs)
                                          putStrLn $ "MsgLen: " ++ show msgLen
                                          let msgId = B.head msgIdBs
                                          if msgLen == 1
                                              then return (msgId, B.empty)
                                              else do
                                                      payload <- readUntilLength sock (msgLen-1)
                                                      return (msgId, payload)
                            Nothing -> error "Invalid message" -- TODO: Handle this better

sendPeerMessage :: Socket -> ByteString -> MessageId -> IO ()
sendPeerMessage sock payload msgId = let size = (fromIntegral $ B.length payload + 1) :: Int32
                                     in send sock $ (LB.toStrict (Bin.encode size) `B.snoc` msgId) `B.append` payload --TODO check if payload is greater than int32? 

handleBitField :: Socket -> IO ByteString -- TODO: Think if the available pieces should be parsed
handleBitField sock = do
                        (msgId, bitField) <- readPeerMessage sock
                        return (if msgId == bitFieldMessageId then bitField
                                  else error "Expected BitField message")

sendInterestedMessage :: Socket -> IO ()
sendInterestedMessage sock = sendPeerMessage sock B.empty interestMessageId


handleUnchoke :: Socket -> IO ()
handleUnchoke sock = do
                      putStrLn "Waiting for unchoke..."
                      (msgId, _) <- readPeerMessage sock
                      putStrLn "Unchoked"
                      if msgId == unchokeMessageId
                        then return ()
                        else error "Expected BitField message"


downloadBlock :: Socket -> Int -> Int -> Int -> IO ByteString
downloadBlock sock pieceIndex blockIndex blockLength = do
                                                          print $ "Downloading block " ++ show blockIndex ++ " offset: " ++ show (blockIndex * blockSize) ++ " of piece: " ++ show pieceIndex
                                                          let msg = foldr (\n bs ->  LB.toStrict (Bin.encode (fromIntegral n :: Int32)) `B.append` bs) B.empty [pieceIndex, blockIndex * blockSize, blockLength]
                                                          -- print $ B16.encode msg
                                                          sendPeerMessage sock msg requestMessageId
                                                          --- 
                                                          (msgId, block) <- readPeerMessage sock
                                                          print $ "Read bytes: " ++ (show $ B.length block)
                                                          if msgId == pieceMessageId then return block
                                                                                     else
                                                                                           do
                                                                                             print $ "Mira que loco, otro id: " ++ show msgId
                                                                                             return B.empty -- TODO: Handle this better
                                                          -- return B.empty

downloadPiece :: Socket -> Int -> Int -> IO ByteString
downloadPiece sock size pieceIdx = do
                                let blocksQty = size `div` blockSize
                                let lastBlockSize = size `mod` blockSize
                                let getSize idx = if idx == blocksQty-1 && lastBlockSize /= 0 then lastBlockSize else blockSize
                                -- TODO: Do in parallel?
                                print $ "Downloading piece. There are " ++ show blocksQty ++ " blocks"
                                print $ "Last block size: " ++ show lastBlockSize
                                -- downloadBlock sock pieceIdx (0 * blockSize) (getSize 0)
                                blockList <- sequence [ downloadBlock sock pieceIdx blockIdx (getSize blockIdx)  | blockIdx <- [0..blocksQty-1] ]
                                return $ B.concat blockList