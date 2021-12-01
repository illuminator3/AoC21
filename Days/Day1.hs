module Days.Day1 (
    solveDay1Part1,
    solveDay1Part2
) where

import Common

solveX :: Show a => ([Int] -> a) -> IO ()
solveX f = getInput >>= putStrLn . show . f . map readInt . lines

solveDay1Part1 :: IO ()
solveDay1Part1 = solveX solve1

solve1 :: (Num a, Ord a) => [a] -> a
solve1 [] = 0
solve1 (x:xs) = solve1' x xs 0

solve1' :: (Num a, Ord a) => a -> [a] -> a -> a
solve1' _ [] = id
solve1' c (x:xs) = solve1' x xs . (+) (if x > c then 1 else 0)

solveDay1Part2 :: IO ()
solveDay1Part2 = solveX solve2

solve2 :: (Num a, Ord a) => [a] -> a
solve2 [] = 0
solve2 xs = solve2' (map sum . groupOf 3 $ xs)

solve2' :: (Num a, Ord a) => [a] -> a
solve2' [] = 0
solve2' (x:xs) = solve1' x xs 0 