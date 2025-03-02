module Tracker (
    getTest,
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

getTest :: IO String
getTest = do
    rsp <- simpleHTTP (getRequest "http://www.haskell.org/")
    getResponseBody rsp

getPeers :: String -> TrackerQueryParams -> IO String -- TrackerResponse
getPeers url params = do
                        let q = "info_hash=" ++ (urlEncode $ B.unpack $ infoHash params) ++ "&peer_id=" ++ (urlEncode $ B.unpack $ peerId params) ++ "&port=" ++ show (port params) ++ "&uploaded=" ++ show (uploaded params) ++ "&downloaded=" ++ show (downloaded params) ++ "&left=" ++ show (left params) ++ "&compact=" ++ show (compact params)
                        rsp <- simpleHTTP $ getRequest (url ++ "?" ++ q)
                        getResponseBody rsp
