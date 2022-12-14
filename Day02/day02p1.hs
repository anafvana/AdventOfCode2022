-- CODE (step 1)
-- Opponent Self    Choice      Choice pts
-- A        X       Rock        1
-- B        Y       Paper       2
-- C        Z       Scissors    3

-- POSSIBLE RESULTS
-- Outcome  Value(s)    Modulo %3
-- Draw     23          2
-- Win      21 or 24    0
-- Loss     22 or 25    1     

import System.IO
import Data.Char (ord)

main = do
    handle <- openFile "day02.txt" ReadMode
    contents <- hGetContents handle
    let pairs = cast contents
    let pts = map points pairs
    print pts
    print $ sum pts
    hClose handle

-- Map file contents to pairs
cast :: String -> [String]
cast s = map (concat . words) $ lines s

-- Pairs to points mapper
points :: [Char] -> Int
points [] = 0
points [_] = 0
    -- Use modulo operator to determine whether relationship between opponent letter and self letter is draw/win/loss and attribute points based on result
points (p:p':ps) = case mod (ord p' - ord p) 3 of
    -- win = 6 pts + 1/2/3 pts for choice
    0 -> ord p' - 81
    -- loss = 1/2/3 pts for choice
    1 -> ord p' - 87
    -- draw = 3 pts + 1/2/3 for choice
    2 -> ord p' - 84
    -- should be impossible otherwise
    _ -> 0
