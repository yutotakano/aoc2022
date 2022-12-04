{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Day04
    ( part1
    , part2
    ) where

import Data.Text qualified as T
import Data.Bifunctor
import Data.Ix

-- >>> part1 "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8" == "2"
-- True
part1 :: T.Text -> T.Text
part1 inputs =
    let
        pairs = map (map (bimap (read @Int . T.unpack) (read @Int . T.unpack . T.drop 1) . T.break (== '-')) . T.split (== ',')) $ T.lines inputs
        containingPairs = filter id $ flip map pairs $ \case
            [(minA, maxA), (minB, maxB)] ->
                (minA >= minB && maxA <= maxB) || (minB >= minA && maxB <= maxA)
            _ -> error "Invalid parse"
    in
        T.pack $ show $ length containingPairs

-- >>> part2 "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8" == "4"
-- True
part2 :: T.Text -> T.Text
part2 inputs =
    let
        pairs = map (map (bimap (read @Int . T.unpack) (read @Int . T.unpack . T.drop 1) . T.break (== '-')) . T.split (== ',')) $ T.lines inputs
        overlappingPairs = filter id $ flip map pairs $ \case
            [(minA, maxA), (minB, maxB)] ->
                (inRange (minB, maxB) minA) || (inRange (minB, maxB) maxA) ||
                (inRange (minA, maxA) minB) || (inRange (minA, maxA) maxB)
            _ -> error "Invalid parse"
    in
        T.pack $ show $ length overlappingPairs
