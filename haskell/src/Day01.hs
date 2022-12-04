{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Day01
    ( part1
    , part2
    ) where

import Data.Text qualified as T
import Data.List qualified as L

-- >>> part1 "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" == "24000"
-- True
part1 :: T.Text -> T.Text
part1 inputs =
    let
        calories = map (sum . map (read @Int . T.unpack) . T.lines) $ T.splitOn "\n\n" inputs
    in
        T.pack $ show $ maximum calories

-- >>> part2 "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" == "45000"
-- True
part2 :: T.Text -> T.Text
part2 inputs =
    let
        calories = map (sum . map (read @Int . T.unpack) . T.lines) $ T.splitOn "\n\n" inputs
    in
        T.pack $ show $ sum $ take 3 $ L.sortBy (flip compare) calories
