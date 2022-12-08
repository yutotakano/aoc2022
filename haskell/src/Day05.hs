{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Day05
    ( part1
    , part2
    ) where

import Data.Text qualified as T
import Data.HashMap.Strict qualified as HM
import Data.Sequence qualified as S
import Data.Char
import Debug.Trace

parseInstruction :: T.Text -> (Int, Int, Int)
parseInstruction input = (numberCrates, fromCol, toCol)
    where
        numberCrates = read $ T.unpack $ T.takeWhile isDigit $ T.drop 5 input
        fromCol = read $ T.unpack $ T.takeWhile isDigit $ T.drop 5 $ T.dropWhile (/= 'f') input
        toCol = read $ T.unpack $ T.takeWhile isDigit $ T.drop 3 $ T.dropWhile (/= 't') input

executeInstructions :: HM.HashMap Int (S.Seq Char) -> [(Int, Int, Int)] -> HM.HashMap Int (S.Seq Char)
executeInstructions crates [] = crates
executeInstructions crates ((0, _, _):xs) = executeInstructions crates xs
executeInstructions crates ((numberCrates, from, to):xs) = executeInstructions newCrates ((numberCrates - 1, from, to):xs)
    where
        newCrates = case fmap S.viewl $ HM.lookup from crates of
            Nothing -> error "no crates in from position, col not exist"
            Just (S.EmptyL) -> error "no crates in from position, empty col"
            Just (top S.:< rest) -> HM.insertWith (\new old -> new S.>< old) to (S.singleton top) $ HM.insert from rest crates

executeInstructions2 :: HM.HashMap Int (S.Seq Char) -> [(Int, Int, Int)] -> HM.HashMap Int (S.Seq Char)
executeInstructions2 crates [] = crates
executeInstructions2 crates ((numberCrates, from, to):xs) = executeInstructions2 newCrates xs
    where
        newCrates = case fmap (S.splitAt numberCrates) $ HM.lookup from crates of
            Nothing -> error "no crates in from position, col not exist"
            Just (moving, remains) -> HM.insertWith (\new old -> new S.>< old) to moving $ HM.insert from remains crates

viewTop :: [Int] -> HM.HashMap Int (S.Seq Char) -> String
viewTop colIdx crates = map (\i -> get $ fmap S.viewl $ HM.lookup i crates) colIdx
    where
        get :: Maybe (S.ViewL Char) -> Char
        get Nothing = ' '
        get (Just (S.EmptyL)) = ' '
        get (Just (top S.:< _)) = top

part1 :: T.Text -> T.Text
part1 inputs =
    let
        inputLines = T.lines inputs
        crates = T.length (head inputLines) `div` 4
        -- foldr flips it
        initialCrates = traceShowId $ foldr (\line m -> foldr (\i m' -> HM.insertWith (S.><) (i + 1) (if T.index line (4 * i + 1) /= ' ' then S.singleton (T.index line (4 * i + 1)) else S.empty) m') m [0..crates]) HM.empty $ takeWhile (\t -> T.length t > 0 && T.head t /= ' ') inputLines
        instructions = dropWhile (\t -> T.length t == 0 || T.head t /= 'm') inputLines
    in
        T.pack $ viewTop [1..crates + 1] $ executeInstructions initialCrates $ (map parseInstruction instructions)

part2 :: T.Text -> T.Text
part2 inputs =
    let
        inputLines = T.lines inputs
        crates = T.length (head inputLines) `div` 4
        initialCrates = foldr (\line m -> foldr (\i m' -> HM.insertWith (S.><) (i + 1) (if T.index line (4 * i + 1) /= ' ' then S.singleton (T.index line (4 * i + 1)) else S.empty) m') m [0..crates]) HM.empty $ takeWhile (\t -> T.length t > 0 && T.head t /= ' ') inputLines
        instructions = dropWhile (\t -> T.length t == 0 || T.head t /= 'm') inputLines
    in
        T.pack $ viewTop [1..crates + 1] $ executeInstructions2 initialCrates (map parseInstruction instructions)
