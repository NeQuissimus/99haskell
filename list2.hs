module List2 where

import List1

-- Problem 11
data Wrapper a = Single a | Multiple (Int, a)
  deriving (Show)

encodeModified :: Eq a => [a] -> [Wrapper a]
encodeModified = map wrap . encode
  where
    wrap (1, x) = Single x
    wrap (n, x) = Multiple (n, x)

-- Problem 12
decodeModified :: Eq a => [Wrapper a] -> [a]
decodeModified [] = []
decodeModified (Single x : xs) = x : decodeModified xs
decodeModified (Multiple (n, x) : xs) = take n (repeat x) ++ decodeModified xs

-- Problem 13
encodeDirect :: Eq a => [a] -> [Wrapper a]
encodeDirect = map wrap . foldr countElems []
  where
    wrap (1, x) = Single x
    wrap (n, x) = Multiple (n, x)
    countElems x [] = [(1, x)]
    countElems x (t@(n, y) : zs)
      | x == y = (n + 1, x) : zs
      | otherwise = (1, x) : t : zs

-- Problem 14
dupli :: [a] -> [a]
dupli xs = xs >>= (\x -> [x, x])

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs = \n -> xs >>= (\x -> take n (repeat x))

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs = \n -> map fst $ filter (\(_, c) -> c `mod` n /= 0) $ zip xs [1..]

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n
  | n <= length xs = (take n xs, drop n xs)
  | otherwise = error "List not long enough!"

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs start stop
  | start >= stop = error "Start index needs to be >= stop index"
  | stop > length xs = error "Stop index needs to be <= length of list"
  | start < 1 = error "Start index needs to be at least 1"
  | otherwise = take (stop - start + 1) $ drop (start - 1) xs
