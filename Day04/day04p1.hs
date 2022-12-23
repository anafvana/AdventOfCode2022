#!/usr/bin/env stack
-- stack script --resolver lts-18.6 --package split

import System.IO
import Data.Char (ord)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    handle <- openFile "day04.txt" ReadMode
    contents <- hGetContents handle
    let ranges = map extractRanges $ stringsToInts $ map splitComma $ lines contents
    print $ sum $ map subSet ranges
    hClose handle

-- Format 2-element list as tuple
tuplify2 :: [a] -> (a, a)
tuplify2 [x,y] = (x,y)

-- Split on , alias
splitComma :: String -> [String]
splitComma = splitOn ","

-- Get numbers from dash-separated string. From "1-2" to (1, 2)
dashedToInts :: String -> (Int, Int)
dashedToInts x = tuplify2 $ map read $ splitOn "-" x

-- Parse array with all dashed strings into all int tuples, paired per couple of elves
stringsToInts :: [[String]] -> [[(Int, Int)]]
stringsToInts [] = []
stringsToInts x = if null h then [] else h : t
    where
      n = filter (not . null) x
      h = map dashedToInts $ head n
      t = stringsToInts (tail n)

-- From pairs of tuples to pairs of ranges 
extractRanges :: [(Int,Int)] -> [[Int]]
extractRanges [] = []
extractRanges [(x,x'), (y,y')] = [x..x']:[[y..y']]

-- Test if (x:xs) is sublist of (y:ys)
subList :: Eq a => [a] -> [a] -> Bool
subList [] []   = True
subList _ []    = False
subList [] _    = True
subList (x:xs) (y:ys) 
    | x == y    = subList xs ys   
    | otherwise = subList (x:xs) ys

-- Test if a is a subset of b or vice-versa
subSet :: Eq a => [[a]]  -> Int
subSet [] = 0
subSet [[]] = 0
subSet [a,b] = if a == b || subList a b || subList b a then 1 else 0