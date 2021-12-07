module Days.Day6 (
    solveDay6Part1,
    solveDay6Part2
) where

import Common

solveDay6Part1 :: IO ()
solveDay6Part1 = solveDay 80

solveDay6Part2 :: IO ()
solveDay6Part2 = solveDay 256

solveDay :: Int -> IO ()
solveDay n = show <$> sum <$> solve n <$> (\list -> map (fromIntegral . flip amount list) [0..9]) <$> map read <$> splitBy "," <$> getInput >>= putStrLn

solve :: Int -> [Integer] -> [Integer]
solve 0 config = config
solve days (valid:config) = solve (days - 1) $ mapEach [id, id, id, id, id, id, (+ valid), id] config ++ [valid]