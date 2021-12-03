module Days.Day3 (
    solveDay3Part1,
    solveDay3Part2
) where

import Common
import Data.List

solveDay3Part1 :: IO ()
solveDay3Part1 = solve lsubcombine msubcombine

solveDay3Part2 :: IO ()
solveDay3Part2 = solve oxygen co2
                where oxygen = run' $ frun '1' mcommonElement
                      co2 = run' $ frun '0' lcommonElement
                      run' f = flip f 0

solve :: ([String] -> String) -> ([String] -> String) -> IO ()
solve f1 f2 = show <$> product <$> map binToDec <$> mapEach [f1, f2] <$> replicate 2 <$> lines <$> getInput >>= putStrLn

frun :: Char -> (String -> Char) -> [String] -> Int -> String
frun _ _ [] _ = []
frun _ _ [x] _ = x
frun base _ [x, y] n = if x !! n == base then x else y
frun base f xs n = flip (frun base f) (n + 1) $ filter ((== common) . (!! n)) xs
                where common = f $ transpose xs !! n