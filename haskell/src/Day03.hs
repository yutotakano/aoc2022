{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Day03
    ( part1
    , part2
    ) where

import Data.Text qualified as T
import Data.HashSet qualified as S
import Data.Ix
import Data.List.Split

-- | Calculate the priority for a given item.
priority :: Char -> Int
priority c
    | p <- (fromEnum c - 96)
    , inRange (1, 26) p = p
    | p <- fromEnum c - 38
    , inRange (27, 52) p = p
    | otherwise = 0

-- >>> part1 "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw" == "157"
-- True
part1 :: T.Text -> T.Text
part1 inputs =
    let
        rucksacks = T.lines inputs
        duplicates = flip map rucksacks $ \rucksack ->
            let
                (firstCompartment, secondCompartment) = T.splitAt (T.length rucksack `div` 2) rucksack
                itemsInFirst = S.fromList (T.unpack firstCompartment)
            in
                head $
                    [ x | x <- T.unpack secondCompartment
                    , S.member x itemsInFirst
                    ]
    in
        T.pack $ show $ sum $ map priority duplicates

-- >>> part2 "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw" == "70"
-- True
part2 :: T.Text -> T.Text
part2 inputs =
    let
        rucksacks = T.lines inputs
        duplicates = flip map (chunksOf 3 rucksacks) $ \group ->
            let
                itemsInFirst = S.fromList (T.unpack $ head group)
                itemsInSecond = S.fromList (T.unpack $ head $ tail group)
            in
                head $
                    [ x | x <- T.unpack (head $ tail $ tail group)
                    , S.member x itemsInFirst
                    , S.member x itemsInSecond
                    ]
    in
        T.pack $ show $ sum $ map priority duplicates
