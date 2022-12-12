import System.IO
import Data.List (sort)

main = do
    let sums = []
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singlelines = lines contents
        grps = groupcals [] singlelines []
        cals = map cast grps
        sums = map sum cals
        maxv = foldr max 0 sums
        max3v = take 3 (reverse (sort sums))
    print maxv
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