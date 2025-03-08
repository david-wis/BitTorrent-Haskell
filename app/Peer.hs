{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Peer (
    handshake
) where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Utils (Address (Address), PeerId, Hash)
import Network.Simple.TCP
import Data.ByteString.Base16 (encode)

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
-- End Constants

handshake :: Address -> Hash -> PeerId -> IO PeerId
handshake (Address ip port) hash pid = connect ip port (handleHandshake hash pid)


handleHandshake :: Hash -> PeerId -> (Socket, SockAddr) -> IO PeerId
handleHandshake hash pid (sock, addr) = do 
                                      let msg = handshakePrefix `B.append` reservedBytes `B.append` hash `B.append` pid
                                      send sock msg
                                      maybeRsp <- recv sock (handshakePrefixLength + reservedBytesLength + hashLength + peerIdLength)
                                      case maybeRsp of 
                                        Just rsp -> let prefix = B.take handshakePrefixLength rsp in
                                                    return (
                                                      if prefix == handshakePrefix 
                                                        then B.drop (handshakePrefixLength + reservedBytesLength + hashLength) rsp
                                                        else error "Invalid response"
                                                    )
                                        Nothing -> error "No response"
