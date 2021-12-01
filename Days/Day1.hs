module Days.Day1 (
    solveDay1Part1,
    solveDay1Part2
) where

import Common

solveX :: Show a => ([Int] -> a) -> IO ()
solveX f = getInput >>= putStrLn . show . f . map readInt . lines

solveDay1Part1 :: IO ()
solveDay1Part1 = solveX solve

solveDay1Part2 :: IO ()
solveDay1Part2 = solveX solve2

solve :: (Num a, Ord a) => [a] -> a
solve [] = 0
solve (x:xs) = solve' x xs 0

solve' :: (Num a, Ord a) => a -> [a] -> a -> a
solve' _ [] = id
solve' c (x:xs) = solve' x xs . (+) (if x > c then 1 else 0)

solve2 :: (Num a, Ord a) => [a] -> a
solve2 [] = 0
solve2 xs = solve (map sum . groupOf 3 $ xs)