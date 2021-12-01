module AoC where

import Days
import Common
import System.IO

main :: IO ()
main = putStr "Enter the day you'd like to run: " >> readInt <$> getLine >>= \day -> putStr "Enter the part you'd like to run: " >> readInt <$> getLine >>= \part -> runDay (findDay day) (toPart part)

toPart :: Int -> Either () ()
toPart 1 = Left ()
toPart 2 = Right ()
toPart _ = error "Invalid part number"