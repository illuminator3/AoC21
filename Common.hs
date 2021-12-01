module Common(
    readInt,
    getInput,
    groupOf
) where

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