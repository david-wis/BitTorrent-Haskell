{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Tracker (
    TrackerQueryParams (TrackerQueryParams),
    TrackerResponse (TrackerResponse),
    interval,
    peers,
    getPeers,
    infoHash,
    peerId,
    port ,
    uploaded ,
    downloaded ,
    left ,
    compact

) where

import Data.ByteString.Char8 (ByteString)
import Data.List (intercalate, find)
import qualified Data.ByteString.Char8 as B
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

import qualified Data.ByteString.Base16 as Base16


import Bencode ( parseBencode, BencodedElem(BencodedDict), bReadInt, bReadString, bencodeGetValue )
import Utils (segmentByteString, Address (Address), readBytesAsInt)

data TrackerQueryParams = TrackerQueryParams {
    infoHash :: ByteString,
    peerId :: ByteString,
    port :: Int,
    uploaded :: Int,
    downloaded :: Int,
    left :: Int,
    compact :: Int
}


data TrackerResponse = TrackerResponse {
    interval :: Int,
    peers :: [Address]
}

-- instance Show TrackerResponse where
--     show trsp = "TrackerResponse { interval = " ++ show (interval trsp) ++ ", peers = " ++ show (peers trsp) ++ " }"



parseAddress :: ByteString -> Address
parseAddress bs = let (ip, port) = B.splitAt 4 bs
                      (intPort, B.uncons -> Nothing) = readBytesAsInt port 2
                  in Address (intercalate "." $ map (show . fromEnum) $ B.unpack ip) (show intPort)

buildTrackerResponse :: BencodedElem -> Maybe TrackerResponse
buildTrackerResponse bed@(BencodedDict _) = do
                                                interv <- bencodeGetValue bed "interval"
                                                intInterv <- bReadInt interv
                                                bePeers <- bencodeGetValue bed "peers"
                                                bsPeers <- bReadString bePeers
                                                return $ TrackerResponse { interval = intInterv , peers = map parseAddress $ segmentByteString bsPeers 6 }
buildTrackerResponse _ = Nothing

-- Possible improvement: unescape unicode chars
encodeUri :: ByteString -> String
encodeUri bs = concatMap (('%' : ) . B.unpack) $ segmentByteString (Base16.encode bs) 2

getPeers :: String -> TrackerQueryParams -> IO TrackerResponse
getPeers url params = do
                        let q = "info_hash=" ++ encodeUri (infoHash params) ++ "&peer_id=" ++ encodeUri (peerId params) ++ "&port=" ++ show (port params) ++ "&uploaded=" ++ show (uploaded params) ++ "&downloaded=" ++ show (downloaded params) ++ "&left=" ++ show (left params) ++ "&compact=" ++ show (compact params)
                        rawBody <- getResponseBody =<< simpleHTTP (getRequest (url ++ "?" ++ q))
                        case buildTrackerResponse =<< parseBencode (B.pack rawBody) of
                             Just trsp -> return trsp
                             Nothing -> error "Invalid Tracker Response"



