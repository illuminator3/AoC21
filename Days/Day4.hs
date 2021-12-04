module Days.Day4 (
    solveDay4Part1,
    solveDay4Part2
) where

import Common
import Data.List

solveDay4Part1 :: IO ()
solveDay4Part1 = solveF solve

solveDay4Part2 :: IO ()
solveDay4Part2 = solveF solve2

solveF :: ([[[Int]]] -> [Int] -> ([[Int]], Int, [Int])) -> IO ()
solveF solve = lines <$> getInputUntil "." >>= \input ->
     let (winningBoard, finalNumber, rolledNumbers) = flip solve (map read . splitBy "," $ head input) $ map (map $ map read . splitBy " ") $ splitByEl [] . map trim $ drop 2 input
     in  putStrLn . show . product . mapEach [sum, head] $ [flip filter (concat winningBoard) $ \x -> not $ elem x rolledNumbers, [finalNumber]]

solve :: [[[Int]]] -> [Int] -> ([[Int]], Int, [Int])
solve [] _ = undefined
solve xs nums = solve' xs nums []

solve' :: [[[Int]]] -> [Int] -> [Int] -> ([[Int]], Int, [Int])
solve' [] _ _ = undefined
solve' xs possible nums = if any (\x -> isSolved newNums x) xs
                            then (head $ findAllSolved xs newNums, rolledNum, newNums)
                            else solve' xs newPossible newNums
                        where (rolledNum, newPossible) = rollNumber possible
                              newNums = rolledNum : nums

solve2 :: [[[Int]]] -> [Int] -> ([[Int]], Int, [Int])
solve2 [] _ = undefined
solve2 xs nums = solve2' xs nums []

solve2' :: [[[Int]]] -> [Int] -> [Int] -> ([[Int]], Int, [Int])
solve2' [] _ _ = undefined
solve2' xs possible nums = if any (\x -> isSolved newNums x) xs
                            then if length xs == 1
                                then (head xs, rolledNum, newNums)
                                else solve2' (xs \\ findAllSolved xs newNums) newPossible newNums
                            else solve2' xs newPossible newNums
                        where (rolledNum, newPossible) = rollNumber possible
                              newNums = rolledNum : nums                              

findAllSolved :: [[[Int]]] -> [Int] -> [[[Int]]]
findAllSolved [] _ = undefined
findAllSolved boards nums = filter (isSolved nums) boards

rollNumber :: [Int] -> (Int, [Int])
rollNumber [] = (0, [])
rollNumber nums = (head nums, drop 1 nums)

isSolved :: [Int] -> [[Int]] -> Bool
isSolved nums = elem True . concat . mapEach (replicate 2 $ map (isSolvedRow nums)) . mapEach [id, transpose] . replicate 2

isSolvedRow :: [Int] -> [Int] -> Bool
isSolvedRow nums row = row `isSubset` nums