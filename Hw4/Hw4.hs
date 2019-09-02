module Hw4.Hw4 where 

import Data.List
-- Exercise 1: Wholemeal programming

-- First function to reimplement
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

-- Second function to reimplement
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- subtracts 2 from each even integer in the list, then takes the product of them
-- more "wholemeal" reimplememntation of fun1
fun1' :: [Integer] -> Integer
fun1' = product . map (+ (-2)) . filter even

-- sums the even numbers of a hailstone sequence
-- hopefully terminates, but who knows!
-- more "wholemeal" reimplememntation of fun2
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate hailstone

hailstone :: Integer -> Integer
hailstone x | even x = x `div` 2 
            | otherwise = 3 * x + 1


-- Exercise 3: More folds!

-- returns true if there are an odd number of true values in input list, false otherwise
xor :: [Bool] -> Bool
xor = foldr (/=) False 


-- Implement map as a fold.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []


-- Exercise 4: Finding primes

-- computes cartesian products (given by exercise)
-- cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- given integer n, returns list of primes up to 2n + 2
-- takes a while to compute the cartesian product of large numbers
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let products     = cartProd [1..n] [1..n]
                      numsToRemove = map (\(i, j) -> i + j + 2 * i * j) products
                      remaining    = [1..n] \\ numsToRemove
                      oddPrimes    = map (\x -> 2 * x + 1) remaining
                      sortedPrimes = sort oddPrimes
                  in sortedPrimes
