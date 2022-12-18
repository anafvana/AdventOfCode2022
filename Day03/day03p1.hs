import System.IO
import Data.Char (ord)

main = do
    handle <- openFile "day03.txt" ReadMode
    contents <- hGetContents handle
    let pts = map (points . matcher . half) $ lines contents
    print $ sum pts
    hClose handle

-- Split string in half
half :: String -> (String, String)
half ln = splitAt (div (length ln) 2) ln

-- Filter elements from 2nd half of the string which can be found in first half. Take head only, to avoid duplicates
matcher :: (String, String) -> Char
matcher (s1, s2) = head $ filter (`elem` s1) s2

-- Calculate priority points
points :: Char -> Int
points c
    -- a-z = 1-26
    | 96 < ord c && ord c < 123 = ord c - 96
    -- A-Z = 27-52
    | 64 < ord c && ord c < 91 = ord c - 38
    | otherwise = 0