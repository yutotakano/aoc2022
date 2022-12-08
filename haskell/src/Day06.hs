{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Day06
    ( part1
    , part2
    ) where

import Data.Text qualified as T
import Data.Set qualified as Set

-- | @findMarkerLoc markerSize remaining counter@ finds the length of the string
-- until where a marker of size @markerSize@ terminates, within the search string
-- @remaining@. The counter should be 0 when calling this function.
findMarkerLoc :: Int -> String -> Int -> Int
findMarkerLoc markerSize remaining counter
    | length remaining < markerSize = error "EOL reached, no start marker found"
    | potentialMarker <- take markerSize remaining
    , Set.size (Set.fromList potentialMarker) == markerSize = (counter + markerSize)
    | otherwise = findMarkerLoc markerSize (tail remaining) (counter + 1)

part1 :: T.Text -> T.Text
part1 inputs = T.pack $ show $ findMarkerLoc 4 (T.unpack inputs) 0

part2 :: T.Text -> T.Text
part2 inputs = T.pack $ show $ findMarkerLoc 14 (T.unpack inputs) 0
