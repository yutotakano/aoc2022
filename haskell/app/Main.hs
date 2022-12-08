{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Advent
import Data.Text qualified as T
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Time.Calendar.MonthDay
import Runner
import System.Exit
import System.Environment

import Day01 qualified
import Day02 qualified
import Day03 qualified
import Day04 qualified
import Day05 qualified
import Day06 qualified
import Day07 qualified
import Day08 qualified

-- | Dynamically generate pattern matches for the day and part 1/2 from the
-- imported modules beginning with "Day".
run :: Int -> Int -> T.Text -> T.Text
run = $(mkRun)

-- | Main function.
main :: IO ()
main = do
    -- Get current year and day of year in the AoC server
    (year, dayOfYear) <- aocServerTime >>= (pure . toOrdinalDate . localDay)
    args <- getArgs
    -- Use the day in December from above, or alternatively the CLI arg if
    -- specified. Exit outside of December.
    dayOfMonth <- case dayOfYearToMonthAndDay (isLeapYear year) dayOfYear of
        (12, dom) -> pure $ if null args then dom else (read $ head args)
        _ -> putStrLn "Works only in December!" >> exitFailure

    sessionToken <- lookupEnv "AOC_SESSION" >>= \case
        Nothing -> putStrLn "AOC_SESSION not specified!" >> exitFailure
        Just x -> pure x
    -- Use a pre-set cache directory, always try to cache, and set a rate-limit
    -- too, based on default values in the Advent package.
    let opts = AoCOpts sessionToken year (Just "cache") False 3000000

    -- Fetch the inputs, and run!
    inputs <- runAoC_ opts $ AoCInput (mkDay_ $ toInteger $ dayOfMonth)
    putStrLn . T.unpack $ run dayOfMonth 1 inputs
    putStrLn . T.unpack $ run dayOfMonth 2 inputs

