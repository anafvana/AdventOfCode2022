import System.IO
import Data.List (sort)

main = do
    let sums = []
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let sums = map (sum . cast) $ groupcals [] (lines contents) [] 
        max3v = take 3 (reverse (sort sums))
    -- Solve step 1 
    print $ foldr max 0 sums
    -- Solve step 2
    print (sum max3v)
    hClose handle

-- Map strings to ints
cast :: [String] -> [Int]
cast = map read

-- Group calorie by elf
groupcals :: [String] -> [String] -> [[String]] -> [[String]]
groupcals acc [] out = out ++ [acc]
groupcals acc (x:xs) out
    | x == "" = groupcals [] xs (out ++ [acc])
    | otherwise = groupcals (acc ++ [x]) xs out