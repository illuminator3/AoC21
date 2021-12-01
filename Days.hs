module Days (
    findDay,
    runDay
) where

import Common
import Data.Map
import Days.Day1

days :: Map Int ((IO (), IO ()))
days = fromList [(1, (solveDay1Part1, solveDay1Part2))]

findDay :: Int -> Maybe ((IO (), IO ()))
findDay = flip Data.Map.lookup days

runDay :: Maybe ((IO (), IO ())) -> Either () () -> IO ()
runDay Nothing _ = putStrLn "Day not found"
runDay (Just day) x = case x of
                        Left  _ -> fst day
                        Right _ -> snd day