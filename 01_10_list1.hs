module List1 where

-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x : xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast = head . tail . reverse

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n - 1)

-- Problem 4
myLength :: [a] -> Int
myLength = foldl (\n _ -> n + 1) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x : xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [x] = [x]
compress (x : y : zs)
  | x == y = compress (y : zs)
  | otherwise = x : compress (y : zs)

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x : xs) = (x : takeWhile (\y -> x == y) xs) : pack (dropWhile (\y -> x == y) xs)

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\ys -> (myLength ys, head ys)) . pack
