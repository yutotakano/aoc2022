{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Day07
    ( part1
    , part2
    ) where

import Data.Text qualified as T
import Data.Char
import Data.Bifunctor (first)
import Data.Tree


data File = File
    { fileSize :: Int
    , fileName :: T.Text
    }
    deriving (Show)

part1 :: T.Text -> T.Text
part1 inputs =
    let
        -- ignore first line since we assume it is always $ cd /
        inputLines = tail $ map (fmap T.strip . T.break (== '\n')) $ T.splitOn "\n$ " inputs
        (dirListing, _) = flip (flip foldl ([], [])) inputLines $ \(lsCommands, currentDir) (cmd, output) ->
            if T.take 2 cmd == "ls" then
                (lsCommands <> [(currentDir, output)], currentDir)
            else if T.take 5 cmd == "cd .." then
                (lsCommands, if T.null (T.drop 5 cmd) then init currentDir else init currentDir <> [T.drop 6 cmd])
            else if T.take 2 cmd == "cd" then
                (lsCommands, currentDir <> [T.drop 3 cmd])
            else
                error ("Undefined command: " <> T.unpack cmd)

        dirListingOnlyFiles :: [([T.Text], [File])]
        dirListingOnlyFiles = map (fmap (map (uncurry File . first (read . T.unpack) . T.break isAlpha) . filter ((&&) <$> ((/= 'd') <$> T.head) <*> (/= "\n")) . T.lines)) dirListing

        rootContents :: [File]
        rootContents = snd $ head $ filter (null . fst) dirListingOnlyFiles

        dirListingSizes :: Tree (T.Text, Int)
        dirListingSizes = flip unfoldTree ([], rootContents) $ \(prefix, files) ->
            ( (T.pack (show prefix), sum $ map fileSize files)
            , filter (\(path, _) -> take (length prefix) path == prefix && length prefix + 1 == length path) dirListingOnlyFiles
            )

        totalDirSizes :: Tree (T.Text, Int) -> Tree (T.Text, Int)
        totalDirSizes (Node (name, size) children) = Node (name, size + sum (map (snd . rootLabel . totalDirSizes) children)) $ map totalDirSizes children

    in
        T.pack $ show $ sum $ foldr (\(_, size) xs -> if size <= 100000 then (size : xs) else xs) [] $ totalDirSizes dirListingSizes

part2 :: T.Text -> T.Text
part2 inputs =
    let
        -- ignore first line since we assume it is always $ cd /
        inputLines = tail $ map (fmap T.strip . T.break (== '\n')) $ T.splitOn "\n$ " inputs
        (dirListing, _) = flip (flip foldl ([], [])) inputLines $ \(lsCommands, currentDir) (cmd, output) ->
            if T.take 2 cmd == "ls" then
                (lsCommands <> [(currentDir, output)], currentDir)
            else if T.take 5 cmd == "cd .." then
                (lsCommands, if T.null (T.drop 5 cmd) then init currentDir else init currentDir <> [T.drop 6 cmd])
            else if T.take 2 cmd == "cd" then
                (lsCommands, currentDir <> [T.drop 3 cmd])
            else
                error ("Undefined command: " <> T.unpack cmd)

        dirListingOnlyFiles :: [([T.Text], [File])]
        dirListingOnlyFiles = map (fmap (map (uncurry File . first (read . T.unpack) . T.break isAlpha) . filter ((&&) <$> ((/= 'd') <$> T.head) <*> (/= "\n")) . T.lines)) dirListing

        rootContents :: [File]
        rootContents = snd $ head $ filter (null . fst) dirListingOnlyFiles

        dirListingSizes :: Tree (T.Text, Int)
        dirListingSizes = flip unfoldTree ([], rootContents) $ \(prefix, files) ->
            ( (T.pack (show prefix), sum $ map fileSize files)
            , filter (\(path, _) -> take (length prefix) path == prefix && length prefix + 1 == length path) dirListingOnlyFiles
            )

        totalDirSizes :: Tree (T.Text, Int) -> Tree (T.Text, Int)
        totalDirSizes (Node (name, size) children) = Node (name, size + sum (map (snd . rootLabel . totalDirSizes) children)) $ map totalDirSizes children

        deletionNeeded = 30000000 - (70000000 - (snd $ rootLabel $ totalDirSizes dirListingSizes))

    in
        T.pack $ show $ minimum $ foldr (\(_, size) xs -> if size >= deletionNeeded then (size : xs) else xs) [] $ totalDirSizes dirListingSizes


