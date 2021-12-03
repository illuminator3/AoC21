module Days (
    findDay,
    runDay
) where

import Common
import Data.Map
import Days.Day1
import Days.Day2
import Days.Day3

days :: Map Int ((IO (), IO ()))
days = fromList [(1, (solveDay1Part1, solveDay1Part2)), (2, (solveDay2Part1, solveDay2Part2)), (3, (solveDay3Part1, solveDay3Part2))]

findDay :: Int -> Maybe ((IO (), IO ()))
findDay = flip Data.Map.lookup days

runDay :: Maybe ((IO (), IO ())) -> Bool -> IO ()
runDay Nothing _ = putStrLn "Day not found"
runDay (Just day) x = if x then fst day else snd day