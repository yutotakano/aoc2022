{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Day02
    ( part1
    , part2
    ) where

import Data.Text qualified as T
import Test.QuickCheck.Arbitrary

data Move = Rock | Paper | Scissors deriving (Eq, Ord, Enum, Show, Bounded)

-- | Create an instance or Arbitrary so we can do simple QuickCheck evaluations.
instance Arbitrary Move where
  arbitrary = arbitraryBoundedEnum

-- | Parse the opponent integer move representation to its Move.
parseOpponent :: Char -> Move
parseOpponent 'A' = Rock
parseOpponent 'B' = Paper
parseOpponent 'C' = Scissors
parseOpponent _ = undefined

-- | Parse my own integer move representation to its Move, for part 1.
parseOwn :: Char -> Move
parseOwn 'X' = Rock
parseOwn 'Y' = Paper
parseOwn 'Z' = Scissors
parseOwn _ = undefined

-- | Calculate the score of a match, given opponent and my moves.
score :: Move -> Move -> Int
score opponentMove ownMove = (fromEnum ownMove + 1) + (((fromEnum ownMove + 1 + 3) - fromEnum opponentMove) `mod` 3) * 3

-- >>> part1 "A Y\nB X\nC Z" == "15"
-- True
part1 :: T.Text -> T.Text
part1 input =
    let
        matches = filter (not . T.null) $ T.split (== '\n') input
        scores = flip map matches $ \match ->
            score (parseOpponent $ T.head match) (parseOwn $ T.last $ match)
    in
        T.pack $ show (sum scores :: Int)

-- | Parse the XYZ characters to an integer representing the match result.
-- This is the same calculation done in @score@, but reversed.
parseResult :: Char -> Int
parseResult 'X' = 0
parseResult 'Y' = 3
parseResult 'Z' = 6
parseResult _ = undefined

-- | @counterMove opponentMove result@ returns a move that when played against
-- @opponentMove@ will create a score of @result@, not taking into account the
-- move-bonus (fromEnum ownMove + 1).
--
-- prop> \(a :: Move, b :: Move) -> counterMove a (score a b - (fromEnum b + 1)) == b
-- +++ OK, passed 100 tests.
counterMove :: Move -> Int -> Move
counterMove opponentMove result = toEnum $ (((result `div` 3) + fromEnum opponentMove) - 1) `mod` 3

-- >>> part2 "A Y\nB X\nC Z" == "12"
-- True
part2 :: T.Text -> T.Text
part2 input =
    let
        matches = filter (not . T.null) $ T.split (== '\n') input
        scores = flip map matches $ \match ->
            let
                opponentMove = parseOpponent (T.head match)
            in
                score opponentMove (counterMove opponentMove $ parseResult $ T.last $ match)
    in
        T.pack $ show (sum scores :: Int)
