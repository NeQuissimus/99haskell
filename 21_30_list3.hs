module List3 where

import System.Random
import Data.List

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] 1 = [x]
insertAt _ [] _ = error "Invalid index"
insertAt x xs n = (take (n - 1) xs) ++ [x] ++ (drop (n - 1) xs)

-- Problem 22
range :: Int -> Int -> [Int]
range from to = [from..to]

-- Problem 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  random <- getStdGen
  return $ take n [ xs !! i | i <- randomRs (0, (length xs) - 1) random]

-- Problem 23 - No duplicates (https://wiki.haskell.org/99_questions/Solutions/23)
rnd_select_unique :: [a] -> Int -> IO [a]
rnd_select_unique lst n = map (lst !!) <$> indices
    where indices = take n . nub . randomRs (0, length lst - 1) <$> getStdGen

-- Problem 24
diff_select :: Int -> Int -> IO [Int]
diff_select n max = rnd_select [1..max] n

-- Problem 25
rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select_unique xs (length xs)

-- Problem 26
combinations :: Int -> [a] -> [[a]]
combinations n xs = filter (\x -> (length x) == n) (subsequences xs)
