{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Torrent(
    TorrentInfo (TorrentInfo),
    hashLength,
    getPieceQuantity,
    fileSize,
    name,
    pieceLength,
    pieces,
    TorrentFile (TorrentFile),
    announce,
    info,
    infoHash,
    getTorrentFile,
    torrentFileToHexHash
) where
import Data.ByteString.Char8 (ByteString, uncons, unsnoc, cons, snoc)
import qualified Data.ByteString.Char8 as B
import Crypto.Hash.SHA1 ( hash )
import qualified Data.ByteString.Base16 as Base16

import Bencode ( parseBencodedValue, BencodedElem(BencodedDict), bReadString, bReadInt, bencodeToByteString, bencodeGetValue)

data TorrentInfo = TorrentInfo {
    fileSize :: Int,
    name :: String,
    pieceLength :: Int,
    pieces :: ByteString
}

data TorrentFile = TorrentFile {
    announce :: String,
    info :: TorrentInfo,
    infoHash :: ByteString
}

instance Show TorrentInfo where
    show (TorrentInfo fileSize name pieceLength pieces) = "TorrentInfo { fileSize = " ++ show fileSize ++ ", name = " ++ show name ++ ", pieceLength = " ++ show pieceLength ++ ", pieces = " ++ (B.unpack $ Base16.encode $ pieces) ++ " }"


instance Show TorrentFile where
    show tf@(TorrentFile announce info infoHash) = "TorrentFile { announce = " ++ show announce ++ ", info = " ++ show info ++ ", infoHash = " ++  torrentFileToHexHash tf ++ "}"


getTorrentInfo :: BencodedElem -> Maybe TorrentInfo
getTorrentInfo bed@(BencodedDict _) = do
                                      fileSize <- bencodeGetValue bed "length"
                                      intFileSize <- bReadInt fileSize
                                      name <- bencodeGetValue bed "name"
                                      strName <- bReadString name
                                      pieceLength <- bencodeGetValue bed "piece length"
                                      intPieceLength <- bReadInt pieceLength
                                      pieces <- bencodeGetValue bed "pieces"
                                      strPieces <- bReadString pieces
                                      return TorrentInfo { fileSize = intFileSize, name = B.unpack strName, pieceLength = intPieceLength, pieces = strPieces}
getTorrentInfo _ = error "Bencode elem is not a dictionary"




getTorrentFile :: BencodedElem -> Maybe TorrentFile
getTorrentFile bed@(BencodedDict _) = do
                                      announce <- bencodeGetValue bed "announce"
                                      strAnnounce <- bReadString announce
                                      info <- bencodeGetValue bed "info"
                                      tiInfo <- getTorrentInfo info
                                      return TorrentFile { announce = B.unpack strAnnounce, info = tiInfo, infoHash = hash $ bencodeToByteString info }
getTorrentFile _ = error "Bencode elem is not a dictionary"

torrentFileToHexHash :: TorrentFile -> String
torrentFileToHexHash tf = B.unpack $ Base16.encode $ infoHash tf

hashLength :: Int
hashLength = 20

getPieceQuantity :: TorrentFile -> Int
getPieceQuantity tf = B.length (pieces $ info tf) `div` hashLength