module Days.Day5 (
    solveDay5Part1,
    solveDay5Part2
) where

import Common
import Text.Parsec hiding (getInput)

solveDay5Part1 :: IO ()
solveDay5Part1 = show <$> length <$> unique <$> removeUniques <$> concat <$> map expand <$> filter (\((x, y), (x', y')) -> x == x' || y == y') <$> map parseLine <$> lines <$> getInput >>= putStrLn

solveDay5Part2 :: IO ()
solveDay5Part2 = mempty

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