module Days.Day2 (
    solveDay2Part1,
    solveDay2Part2
) where

import Common

solveDay2Part1 :: IO ()
solveDay2Part1 = solvePart False

solveDay2Part2 :: IO ()
solveDay2Part2 = solvePart True

solvePart :: Bool -> IO ()
solvePart part = toInstructions . lines <$> getInput >>= putStrLn . show . product . drop 1 . tripleToList . flip solvePart' part

solvePart' :: [(Char, Int)] -> Bool -> (Int, Int, Int)
solvePart' instr part = runInstructions instr 0 0 0 part

runInstructions :: [(Char, Int)] -> Int -> Int -> Int -> Bool -> (Int, Int, Int)
runInstructions [] aim x y sp = (aim, x, y)
runInstructions (f:fs) aim x y sp = runInstructions fs naim nx ny sp
                            where (naim, nx, ny) = runInstruction f aim x y sp

runInstruction :: (Char, Int) -> Int -> Int -> Int -> Bool -> (Int, Int, Int)
-- first part
runInstruction ('u', a) aim x y False = (aim, x, y - a)
runInstruction ('d', a) aim x y False = (aim, x, y + a)
runInstruction ('f', a) aim x y False = (aim, x + a, y)
-- second part
runInstruction ('u', a) aim x y True = (aim - a, x, y)
runInstruction ('d', a) aim x y True = (aim + a, x, y)
runInstruction ('f', a) aim x y True = (aim, x + a, y + (aim * a))

toInstructions :: [String] -> [(Char, Int)]
toInstructions [] = []
toInstructions (x:xs) = toInstruction x : (toInstructions xs)

toInstruction :: String -> (Char, Int)
toInstruction s = (s !! 0, read $ words s !! 1)