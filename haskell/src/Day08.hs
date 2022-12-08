{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Day08
    ( part1
    , part2
    ) where

import Data.Text qualified as T
import Data.Char
import Data.Array.Repa as R
import Prelude as P

mapX :: (Int -> Int) -> DIM2 -> DIM2
mapX f sh
    | shList <- listOfShape sh
    , [x, y] <- shList = shapeOfList [f x, y]
    | otherwise = error "Shape of non-DIM2 passed in!"

mapY :: (Int -> Int) -> DIM2 -> DIM2
mapY f sh
    | shList <- listOfShape sh
    , [x, y] <- shList = shapeOfList [x, f y]
    | otherwise = error "Shape of non-DIM2 passed in!"

takeWhileAndNext :: (a -> Bool) -> [a] -> [a]
takeWhileAndNext cond = (\(firstPart, secondPart) -> firstPart <> (take 1 secondPart)) . span cond

part1 :: T.Text -> T.Text
part1 inputs =
    let
        intGrid = P.map (P.map digitToInt . T.unpack) $ T.lines inputs
        gridHeight = length intGrid
        gridWidth = length $ head intGrid
        gridSize = ix2 gridWidth gridHeight
        intArray = fromListUnboxed gridSize $ concat intGrid
        visibleArray = fromFunction gridSize $ \ix -> or $ P.map (foldAllS (&&) True . R.map (\x -> x < (intArray ! ix)))
            [ extract (mapX (const 0) ix) (mapY (const 1) ix) intArray
            , extract (mapY (const 0) ix) (mapX (const 1) ix) intArray
            , extract (mapX (+ 1) ix) (shapeOfList $ (\[x, _] -> [(gridWidth - x - 1), 1]) $ listOfShape $ ix) intArray
            , extract (mapY (+ 1) ix) (shapeOfList $ (\[_, y] -> [1, (gridHeight - y - 1)]) $ listOfShape $ ix) intArray
            ]
    in
        T.pack $ show $ sumAllS $ R.map fromEnum visibleArray

part2 :: T.Text -> T.Text
part2 inputs =
    let
        intGrid = P.map (P.map digitToInt . T.unpack) $ T.lines inputs
        gridHeight = length intGrid
        gridWidth = length $ head intGrid
        gridSize = ix2 gridWidth gridHeight
        intArray = fromListUnboxed gridSize $ concat intGrid
        scenicScoreArray = fromFunction gridSize $ \ix ->
            product $ P.map (length . takeWhileAndNext (< (intArray ! ix)))
                [ reverse $ toList $ extract (mapX (const 0) ix) (mapY (const 1) ix) intArray
                , reverse $ toList $ extract (mapY (const 0) ix) (mapX (const 1) ix) intArray
                , toList $ extract (mapX (+ 1) ix) (shapeOfList $ (\[x, _] -> [(gridWidth - x - 1), 1]) $ listOfShape $ ix) intArray
                , toList $ extract (mapY (+ 1) ix) (shapeOfList $ (\[_, y] -> [1, (gridHeight - y - 1)]) $ listOfShape $ ix) intArray
                ]
    in
        T.pack $ show $ foldAllS max 0 $ scenicScoreArray
