
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Args(
    ArgsInfo(..),
    loadArgs
) where

import Utils(Path)

data ArgsInfo = ArgsInfo {
    inputPath :: Path,
    outputPath :: Path,
    threadsPerPeer :: Int,
    peerCount :: Maybe Int
}

loadArgs :: [String] -> Maybe ArgsInfo
loadArgs as@[_, _] = loadArgs (as ++ ["5"])
loadArgs as@[inpPath, outPath, tPerPeer] = Just $ ArgsInfo {
    inputPath = inpPath,
    outputPath = outPath,
    threadsPerPeer = read tPerPeer,
    peerCount = Nothing
}
loadArgs [inpPath, outPath, tPerPeer, pCount] = Just $ ArgsInfo {
    inputPath = inpPath,
    outputPath = outPath,
    threadsPerPeer = read tPerPeer,
    peerCount = Just (read pCount)
}
loadArgs _ = Nothing
