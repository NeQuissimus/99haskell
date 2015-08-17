module Arithmetic where

-- Problem 31
isPrime :: Int -> Bool
isPrime n = foldr (&&) True (map (\x -> (mod n x) /= 0) [2..n-1])

-- Problem 32
euclid_gcd :: Int -> Int -> Int
euclid_gcd n m
  | m == 0 = n
  | otherwise = euclid_gcd m (mod n m)

-- Problem 33
coprime :: Int -> Int -> Bool
coprime n m = 1 == (euclid_gcd n m)
