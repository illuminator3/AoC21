module Days.Day7 (
    solveDay7Part1,
    solveDay7Part2
) where

import Common

solveDay7Part1 :: IO ()
solveDay7Part1 = solvePartWith $ \k x -> abs $ k - x

solveDay7Part2 :: IO ()
solveDay7Part2 = solvePartWith $ \k x -> sum [1..abs $ k - x]

solvePartWith :: (Int -> Int -> Int) -> IO ()
solvePartWith f = show . solve (solveWith f) . map read . splitBy "," <$> getInput >>= putStrLn

solve :: ([Int] -> [Int] -> Int) -> [Int] -> Int
solve f xs = f [minimum xs..maximum xs] xs

solveWith :: (Int -> Int -> Int) -> [Int] -> [Int] -> Int
solveWith f [] _ = maxBound
solveWith f (x:xs) fx = if next < curr then next else curr
                            where next = solveWith f xs fx
                                  curr = sum $ map (\k -> f k x) fx