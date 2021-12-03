module Days.Day3 (
    solveDay3Part1,
    solveDay3Part2
) where

import Common

solveDay3Part1 :: IO ()
solveDay3Part1 = show <$> product <$> map binToDec <$> mapEach [lsubcombine, msubcombine] <$> replicate 2 <$> lines <$> getInput >>= putStrLn

solveDay3Part2 :: IO ()
solveDay3Part2 = mempty