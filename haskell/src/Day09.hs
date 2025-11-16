{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Day09
    ( part1
    , part2
    ) where

import Data.Text qualified as T
import Data.Bifunctor
import Lens.Micro
import Lens.Micro.TH
import Data.List ( nub )
import Debug.Trace

data Direction = U | D | L | R deriving (Show, Eq, Read)

data Coord = Coord
    { _x :: Int
    , _y :: Int
    }
    deriving (Show, Eq)

data State = State
    { _visitedCells :: [Coord]
    , _tailPos :: Coord
    , _headPos :: Coord
    }
    deriving (Eq)

makeLenses ''Coord
makeLenses ''State

instance Show State where
    show state =
        let
            xCoords = (state ^. tailPos . x):(state ^. headPos . x):(state ^.. visitedCells . traverse . x)
            yCoords = (state ^. tailPos . y):(state ^. headPos . y):(state ^.. visitedCells . traverse . y)
        in unlines $ reverse
            [
                [ if isHead then 'H' else if isTail then 'T' else if isVisited then '#' else ' '
                | i <- [minimum xCoords..maximum xCoords]
                , let isHead = state ^. headPos == Coord i j
                , let isTail = state ^. tailPos == Coord i j
                , let isVisited = Coord i j `elem` state ^. visitedCells
                ]
            | j <- [minimum yCoords..maximum yCoords]
            ]

findOffset :: Coord -> Coord -> [Direction]
findOffset orig dest
    | -- within 3x3 square
      ((dest ^. x) - (orig ^. x)) ^ (2 :: Integer) <= 1
    , ((dest ^. y) - (orig ^. y)) ^ (2 :: Integer) <= 1
    = []
    | -- same axis 2 apart on x
      ((dest ^. x) - (orig ^. x)) ^ (2 :: Integer) > 1
    , (dest ^. y) == (orig ^. y)
    = if dest ^. x > orig ^. x then [R] else [L]
    | -- same axis 2 apart on y
      ((dest ^. y) - (orig ^. y)) ^ (2 :: Integer) > 1
    , (dest ^. x) == (orig ^. x)
    = if dest ^. y > orig ^. y then [U] else [D]
    | -- knight move away
      otherwise
    = [if dest ^. y > orig ^. y then U else D, if dest ^. x > orig ^. x then R else L]

moveCoord :: Direction -> Coord -> Coord
moveCoord U = y +~ 1
moveCoord D = y -~ 1
moveCoord L = x -~ 1
moveCoord R = x +~ 1

followTail :: State -> State
followTail state
    | dirs <- findOffset (state ^. tailPos) (state ^. headPos)
    = over visitedCells (state ^. tailPos :) $ over tailPos (moveCoord dir) state
    | otherwise = state

evalMove :: State -> Direction -> State
evalMove state direction = traceShowId $ followTail $ over headPos (moveCoord direction) state

part1 :: T.Text -> T.Text
part1 input =
    let
        moves = concatMap (uncurry (flip replicate) . bimap read read . splitAt 1 . T.unpack) $ T.lines input
        finalState = foldl evalMove (State [Coord 0 0] (Coord 0 0) (Coord 0 0)) moves
    in
        T.pack $ show $ length $ nub $ finalState ^. visitedCells

part2 :: T.Text -> T.Text
part2 = undefined
