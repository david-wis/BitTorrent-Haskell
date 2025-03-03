module Tracker (
    TrackerQueryParams (TrackerQueryParams),
    TrackerResponse (TrackerResponse),
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
import qualified Data.ByteString.Char8 as B
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.HTTP.Base (urlEncodeVars, urlEncode)
import Network.URI.Encode
import qualified Data.ByteString.Base16 as Base16

import Utils (segmentBytestring)

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
    peers :: ByteString
}

-- Possible improvement: unescape unicode chars
encodeUri :: ByteString -> String
encodeUri bs = concatMap (('%' : ) . B.unpack) $ segmentBytestring (Base16.encode bs) 2

getPeers :: String -> TrackerQueryParams -> IO String -- TrackerResponse
getPeers url params = do
                        let q = "info_hash=" ++ (encodeUri $ infoHash params) ++ "&peer_id=" ++ (B.unpack $ peerId params) ++ "&port=" ++ show (port params) ++ "&uploaded=" ++ show (uploaded params) ++ "&downloaded=" ++ show (downloaded params) ++ "&left=" ++ show (left params) ++ "&compact=" ++ show (compact params)
                        rsp <- simpleHTTP $ getRequest (url ++ "?" ++ q)
                        getResponseBody rsp
                        
