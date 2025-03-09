{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.ByteString.Base16 (encode) -- TODO: Use with qualified
import qualified Data.Binary as Bin

import Torrent (TorrentFile,  infoHash, pieceLength, info)

type MessageId = Char

unchokeMessageId :: MessageId
unchokeMessageId = '\1'

interestMessageId :: MessageId
interestMessageId = '\2'

bitFieldMessageId :: MessageId
bitFieldMessageId = '\5'

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
                                                    putStrLn $ "Connection successful to peer with pid: " ++ B.unpack (encode peerId)
                                                    handleBitField sock
                                                    sendInterestedMessage sock
                                                    handleUnchoke sock
                                                    let size = pieceLength $ info tf
                                                    let blocks = size `div` blockSize
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


readPeerMessage :: Socket -> IO (MessageId, ByteString)
readPeerMessage sock = do
                          maybeRsp <- recv sock 5
                          case maybeRsp of
                            Just bs ->  do
                                          let (msgLen, msgIdBs) = readBytesAsInt bs 4
                                          putStrLn $ B.unpack $ encode bs
                                          putStrLn $ show msgLen
                                          let msgId = B.head msgIdBs
                                          if msgLen == 0 
                                              then return (msgId, B.empty)
                                              else do
                                                      maybeRsp' <- recv sock msgLen
                                                      case maybeRsp' of 
                                                        Just payload -> return (msgId, payload)
                                                        Nothing -> error "Invalid Message"
                            Nothing -> error "Invalid message" -- TODO: Handle this better

sendPeerMessage :: Socket -> ByteString -> MessageId -> IO ()
sendPeerMessage sock payload msgId = let size = (fromIntegral $ B.length payload + 1) :: Int32
                                     in send sock $ ((LB.toStrict $ Bin.encode size) `B.snoc` msgId) `B.append` payload --TODO check if payload is greater than int32? 

handleBitField :: Socket -> IO ByteString -- TODO: Think if the available pieces should be parsed
handleBitField sock = do
                        (msgId, bitField) <- readPeerMessage sock
                        return (if msgId == bitFieldMessageId then bitField 
                                  else error "Expected BitField message")

sendInterestedMessage :: Socket -> IO ()
sendInterestedMessage sock = sendPeerMessage sock B.empty interestMessageId


handleUnchoke :: Socket -> IO ()
handleUnchoke sock = do
                      (msgId, _) <- readPeerMessage sock
                      if msgId == unchokeMessageId
                        then return ()
                        else error "Expected BitField message"


-- downloadBlock :: Socket -> Int -> IO ByteString
-- downloadBlock sock index = do
--                              send sock 
                            