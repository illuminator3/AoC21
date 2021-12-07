module Days.Day5 (
    solveDay5Part1,
    solveDay5Part2
) where

import Common
import Text.Parsec hiding (getInput)
import Data.List

solveDay5Part1 :: IO ()
solveDay5Part1 = show <$> length <$> unique <$> removeUniques <$> concat <$> map expand <$> filter (\((x, y), (x', y')) -> x == x' || y == y') <$> map parseLine <$> lines <$> getInput >>= putStrLn

solveDay5Part2 :: IO ()
solveDay5Part2 = show <$> length <$> unique <$> removeUniques <$> concat . concat <$> mapEach [map expand, map diagonals'] <$> mapEach [filter (\((x, y), (x', y')) -> x == x' || y == y'), filter (\((x, y), (x', y')) -> x /= x' && y /= y')] <$> replicate 2 <$> map parseLine <$> lines <$> getInput >>= putStrLn

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine s = get $ parse lineParser "" s
    where
        lineParser = do
            x <- read <$> many1 digit
            char ','
            y <- read <$> many1 digit
            string " -> "
            x' <- read <$> many1 digit
            char ','
            y' <- read <$> many1 digit
            return ((x, y), (x', y'))
        get (Right x) = x

expand :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
expand ((x, y), (x', y')) = unique $ [(x, y) | x <- [mx..mx']] ++ [(x', y) | y <- [my..my']] ++ [(x, y') | x <- [mx..mx']] ++ [(x', y') | y <- [my..my']]
                            where mx = min x x'
                                  mx' = max x x'
                                  my = min y y'
                                  my' = max y y'


diagonals' :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
diagonals' ((x, y), (x', y')) = diagonals x y x' y'

-- this function hurts my eyes
diagonals :: Int -> Int -> Int -> Int -> [(Int, Int)]
diagonals x y x' y' = if x > x'
                        then if y > y'
                            then (x, y) : diagonals (x - 1) (y - 1) x' y'
                            else if y < y'
                                then (x, y) : diagonals (x - 1) (y + 1) x' y'
                                else (x, y) : diagonals (x - 1) y' x' y'
                        else if x < x'
                            then if y > y'
                                then (x, y) : diagonals (x + 1) (y - 1) x' y'
                                else if y < y'
                                    then (x, y) : diagonals (x + 1) (y + 1) x' y'
                                    else (x, y) : diagonals (x + 1) y' x' y'
                            else if y > y'
                                then (x, y) : diagonals x (y - 1) x' y'
                                else if y < y'
                                    then (x, y) : diagonals x (y + 1) x' y'
                                    else [(x, y)]