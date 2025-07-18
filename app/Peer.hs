{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Peer (
    connectToPeer,
    -- disconnectFromPeer,
    downloadPiece
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Char8 (ByteString)
import Data.Word (Word8)
import Data.Int (Int32)
import Utils ( Address(Address),
      BitField,
      Hash,
      PeerId,
      intToByteString,
      readBytesAsInt,
      PieceIndex,
      BlockIndex )
import Network.Simple.TCP (connect, connectSock, closeSock, Socket, SockAddr, send, recv)
import qualified Data.ByteString.Base16 as B16
import qualified Data.Binary as Bin
import Crypto.Hash.SHA1 ( hash )
import System.IO
import Control.Exception (finally, catch, SomeException)

import Torrent (TorrentFile,  infoHash, pieceLength, info, pieces, fileSize, hashLength)
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

peerIdLength :: Int
peerIdLength = 20

blockSize :: Int
blockSize = 16384 -- 16 KiB
-- End Constants

connectToPeer :: Address -> TorrentFile -> PeerId -> (BitField -> Socket -> IO ()) -> IO Bool
connectToPeer (Address ip port) tf selfPid callback =
  (connect ip port (handlePeerConnection tf selfPid callback) >> return True)
    `catch` \(e :: SomeException) -> return False


handlePeerConnection :: TorrentFile -> PeerId -> (BitField -> Socket -> IO ()) -> (Socket, SockAddr) -> IO ()
handlePeerConnection tf selfPid callback (sock, _) = do
                                                    peerId <- handleHandshake (infoHash tf) selfPid sock
                                                    bitField <- handleBitField sock
                                                    sendInterestedMessage sock
                                                    handleUnchoke sock
                                                    callback bitField sock


-- | Handles the handshake with the peer, sending the info hash and receiving the peer ID
handleHandshake :: Hash -> PeerId -> Socket -> IO PeerId
handleHandshake hash selfPid sock = do
                                  let msg = handshakePrefix `B.append` reservedBytes `B.append` hash `B.append` selfPid
                                  send sock msg
                                  maybeRsp <- recv sock (handshakePrefixLength + reservedBytesLength + hashLength + peerIdLength)
                                  case maybeRsp of
                                    Just response ->
                                                let responsePrefix = B.take handshakePrefixLength response in -- \19BitTorrent protocol
                                                return (
                                                  if responsePrefix == handshakePrefix
                                                  then B.drop (handshakePrefixLength + reservedBytesLength + hashLength) response -- The peer ID is at the end of the response
                                                  else error "Invalid response"
                                                )
                                    Nothing -> error "No response"


readUntilLength :: Socket -> Int -> IO ByteString
readUntilLength sock n = do
                            maybeRsp <- recv sock n
                            case maybeRsp of
                              Just rsp -> let rspLen = B.length rsp
                                              remainingLen = n - rspLen
                                          in if remainingLen > 0 
                                             then do
                                                    remaining <- readUntilLength sock remainingLen
                                                    return (rsp `B.append` remaining)
                                             else return rsp
                              Nothing -> error "Failure reading from socket"


readPeerMessage :: Socket -> IO (MessageId, ByteString)
readPeerMessage sock = do
                          maybeRsp <- recv sock 5 -- Read length + message id
                          case maybeRsp of
                            Just bs ->  do
                                          let (msgLen, msgIdBs) = readBytesAsInt bs 4
                                          let msgId = B.head msgIdBs -- Convert to Char
                                          if msgLen == 1
                                          then return (msgId, B.empty)
                                          else do
                                                  payload <- readUntilLength sock (msgLen-1)
                                                  return (msgId, payload)
                            Nothing -> error "Invalid message"

sendPeerMessage :: Socket -> ByteString -> MessageId -> IO ()
sendPeerMessage sock payload msgId = let size = (fromIntegral $ B.length payload + 1) :: Int32
                                     in send sock $ (LB.toStrict (Bin.encode size) `B.snoc` msgId) `B.append` payload

-- | Fetches the BitField of the peer
handleBitField :: Socket -> IO BitField
handleBitField sock = do
                        (msgId, bitField) <- readPeerMessage sock
                        return (if msgId == bitFieldMessageId then bitField
                                  else error "Expected BitField message")


-- | Sends an interested message to the peer (empty payload)
sendInterestedMessage :: Socket -> IO ()
sendInterestedMessage sock = sendPeerMessage sock B.empty interestMessageId


-- | Waits for an unchoke message from the peer
handleUnchoke :: Socket -> IO ()
handleUnchoke sock = do
                      (msgId, _) <- readPeerMessage sock
                      if msgId == unchokeMessageId
                      then return ()
                      else error "Expected BitField message"

-- 
readBlock :: Socket -> PieceIndex -> BlockIndex -> Int -> IO ByteString
readBlock sock pieceIndex blockIndex actualBlockSize = do
                                                          (msgId, payload) <- readPeerMessage sock
                                                          let block = B.drop (2*4) payload -- The payload consists of index, begin, block
                                                          -- print $ "Read bytes: " ++ (show $ B.length block)
                                                          if msgId == pieceMessageId  -- Possible optimization: act differently when a choked is received
                                                          then return block
                                                          else readBlock sock pieceIndex blockIndex actualBlockSize
                                                          

downloadBlock :: Socket -> PieceIndex -> BlockIndex -> Int -> IO ByteString
downloadBlock sock pieceIndex blockIndex actualBlockSize = do
                                                          -- print $ "Downloading piece " ++ show pieceIndex ++ " block " ++ show blockIndex ++ " offset: " ++ show (blockIndex * blockSize)
                                                          -- Encode the request message
                                                          let msg = foldr (B.append . intToByteString) B.empty [pieceIndex, blockIndex * blockSize, actualBlockSize]
                                                          -- print $ B16.encode msg
                                                          sendPeerMessage sock msg requestMessageId
                                                          --- 
                                                          readBlock sock pieceIndex blockIndex actualBlockSize

downloadPiece :: Socket -> Int -> PieceIndex -> ByteString -> IO ByteString
downloadPiece sock size pieceIdx pieceHash = do
                                                let lastBlockSize = size `mod` blockSize
                                                let blocksQty = size `div` blockSize + if lastBlockSize > 0 then 1 else 0
                                                let getSize idx = if idx /= blocksQty-1 || lastBlockSize == 0 then blockSize else lastBlockSize
                                                -- Possible improvement: pipelining
                                                blockList <- sequence [ downloadBlock sock pieceIdx blockIdx actualBlockSize | blockIdx <- [0..blocksQty-1],
                                                                                                                               let actualBlockSize = getSize blockIdx,
                                                                                                                               actualBlockSize > 0]
                                                let result = B.concat blockList
                                                let calculatedHash = hash result
                                                if calculatedHash /= pieceHash
                                                then do
                                                        print $ "calculated: " ++ B.unpack calculatedHash  ++ " , expected: " ++ B.unpack pieceHash
                                                        error "Hash does not match"
                                                else return result
