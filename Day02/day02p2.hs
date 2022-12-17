-- CODE
-- Letter   Choice      Choice pts
-- A        Rock        1
-- B        Paper       2
-- C        Scissors    3

-- WINNING STRATEGY
-- Opponent Self
-- A        B    
-- B        C    
-- C        A    

-- CODE (step 2)
-- Self     Outcome     Choice
-- X        Lose        Opponent + 2
-- Y        Draw        Opponent
-- Z        Win         Opponent + 1

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

-- Pairs to points mapper*
points :: [Char] -> Int
points [] = 0
points [_] = 0
points (p:p':ps) = case p' of
    -- loss = 1/2/3 pts for choice
    'X' -> (mod (ord p) 3) + 1
    -- draw = 3 pts + 1/2/3 for choice
    'Y' -> 3 + (mod (ord p + 1) 3) + 1
    -- win = 6 pts + 1/2/3 pts for choice
    'Z' -> 6 + (mod (ord p + 2) 3) + 1
    -- should be impossible otherwise
    _ -> 0

-- * How does this actually work?
-- p is the opponent's choice
-- p' is the expected outcome
-- ps is expected to be []
-- By taking the modulo 3 of p ('A', 'B' or 'C'), we can obtain a value between 0 and 2. 
    -- Afterwards, we add 1 to the result of the modulo so that the values will be between 1 and 3 (ergo, the same as the possible number of points per rock/paper/scissor)
-- But we can't just take the modulo of p. Depending on the expected result of the match, we will need to use a "variant" of p. Consider the options for p as a circular rock-paper-scissors-rock-paper-...
-- In order to determine the "variant" of p, the following table must be considered:
    -- RESULT   OPPONENT PTS    SELF PTS
    -- Draw     x               x
    -- Win      x               x + 1
    -- Loss     x               x + 2
-- However, mod (ord 'A') 3 = mod 65 3 = 2. If we add the subsequent +1 to that (necessary to ensure our point range is 1-3), we have that 'A' = 3 points.
    -- If we think about the points as a circle, it is always 1-2-3-1-2-3-...
    -- Thus, if we want to shift 'A' back to being 1 point, all we need to do is add +1 in the modulo operation
-- The resulting table is
    -- RESULT   OPPONENT PTS    SELF PTS    MOD
    -- Draw     x               x           mod (ord p + 1) 3
    -- Win      x               x + 1       mod (ord p + 2) 3
    -- Loss     x               x + 2       mod (ord p + 3) 3 / mod (ord p + 0) 3
-- In summary:
    -- 'Y'          -> 3 +              (mod (ord p + 1) 3)                         + 1
    -- result_code  -> result_points +  (mod (ord opponent_code + mod_adjust) 3)    + point_adjust