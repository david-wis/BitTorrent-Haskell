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
import Data.Bits (shiftL)

import qualified Data.ByteString.Base16 as Base16


import Bencode ( parseBencodedValue, BencodedElem(BencodedDict), bReadInt, bReadString, bencodeGetValue )
import Utils (segmentBytestring, Address (Address))

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

instance Show TrackerResponse where
    show trsp = "TrackerResponse { interval = " ++ show (interval trsp) ++ ", peers = " ++ show (peers trsp) ++ " }"



parseAddress :: ByteString -> Address
parseAddress bs = let (ip, port) = B.splitAt 4 bs
                      (B.uncons -> Just (highPort, B.uncons -> Just (lowPort, _))) = port
                  in Address (intercalate "." $ map (show . fromEnum) $ B.unpack ip) (show $  fromEnum highPort `shiftL` 8 + fromEnum lowPort)

buildTrackerResponse :: BencodedElem -> Maybe TrackerResponse
buildTrackerResponse bed@(BencodedDict _) = do
                                                interv <- bencodeGetValue bed "interval"
                                                intInterv <- bReadInt interv
                                                bePeers <- bencodeGetValue bed "peers"
                                                bsPeers <- bReadString bePeers
                                                return $ TrackerResponse { interval = intInterv , peers = map parseAddress $ segmentBytestring bsPeers 6 }

-- Possible improvement: unescape unicode chars
encodeUri :: ByteString -> String
encodeUri bs = concatMap (('%' : ) . B.unpack) $ segmentBytestring (Base16.encode bs) 2

getPeers :: String -> TrackerQueryParams -> IO TrackerResponse
getPeers url params = do
                        let q = "info_hash=" ++ encodeUri (infoHash params) ++ "&peer_id=" ++ encodeUri (peerId params) ++ "&port=" ++ show (port params) ++ "&uploaded=" ++ show (uploaded params) ++ "&downloaded=" ++ show (downloaded params) ++ "&left=" ++ show (left params) ++ "&compact=" ++ show (compact params)
                        rsp <- simpleHTTP $ getRequest (url ++ "?" ++ q)
                        rawBody <- getResponseBody rsp
                        let body = fst $ parseBencodedValue $ B.pack rawBody
                        case buildTrackerResponse body of
                            Just trsp -> return trsp
                            Nothing -> error "Invalid Tracker Response"



