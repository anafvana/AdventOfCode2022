#!/usr/bin/env stack
-- stack script --resolver lts-18.6 --package split

import System.IO
import Data.Char (ord)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
    handle <- openFile "day03.txt" ReadMode
    contents <- hGetContents handle
    let matches = map matcher $ chunksOf 3 (lines contents)
    print $ sum $ map points matches 
    hClose handle


-- Filter elements from second string if present on first string. Then filter result if present on third string. Return head only to avoid duplicates
matcher :: [String] -> Char
matcher (s1:s2:s3:rest) = head $ filter (`elem` filter (`elem` s1) s2) s3
matcher xs = '_'

-- Calculate priority points
points :: Char -> Int
points c
    -- a-z = 1-26
    | 96 < ord c && ord c < 123 = ord c - 96
    -- A-Z = 27-52
    | 64 < ord c && ord c < 91 = ord c - 38
    | otherwise = 0