{-# LANGUAGE ScopedTypeVariables #-}

module Common(
    readInt,
    getInput,
    groupOf,
    pairToList,
    tripleToList,
    lsubcombine,
    msubcombine,
    lcommonElement,
    mcommonElement,
    binToDec,
    mapEach
) where

import Data.Char
import Data.List

readInt :: String -> Int
readInt = read

getInput :: IO String
getInput = getLine >>= \line -> if null line
                                    then return ""
                                    else getInput >>= \rest -> return $ line ++ "\n" ++ rest

groupOf :: Int -> [a] -> [[a]]
groupOf n xs
    | length xs < n = []
    | otherwise = take n xs : groupOf n (drop 1 xs)

pairToList :: (a, a) -> [a]
pairToList (a, b) = [a, b]

tripleToList :: (a, a, a) -> [a]
tripleToList (a, b, c) = [a, b, c]

subcombine :: forall a. Ord a => ([a] -> a) -> [[a]] -> [a]
subcombine _ [] = []
subcombine xce ls = subcombine' . transpose $ ls
    where
        subcombine' :: Ord a => [[a]] -> [a]
        subcombine' [] = []
        subcombine' (x:xs) = xce x : subcombine' xs

msubcombine :: Ord a => [[a]] -> [a]
msubcombine = subcombine mcommonElement

mcommonElement :: Ord a => [a] -> a
mcommonElement = snd . maximum . commonElement

lsubcombine :: Ord a => [[a]] -> [a]
lsubcombine = subcombine lcommonElement

lcommonElement :: Ord a => [a] -> a
lcommonElement = snd . minimum . commonElement

commonElement :: Ord a => [a] -> [(Int, a)]
commonElement = map (\xs -> (length xs, head xs)) . group . sort

binToDec :: String -> Int
binToDec s = foldl (\acc x -> acc * 2 + digitToInt x) 0 s

mapEach :: [(a -> b)] -> [a] -> [b]
mapEach _ [] = []
mapEach fs xs = map (\(f, x) -> f x) $ zip fs xs