{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
module Hw6.Fibonacci where 


-- Ezercise 1
-- returns fibonacci number of input
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

testFib :: Bool
testFib = map fib (take 15 [0..]) == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]


-- returns list of all fibonacci numbers
-- might take a century to get the 100th number
fibs1 :: [Integer]
fibs1 = map fib [0..]

testFib1 :: Bool
testFib1 = take 15 fibs1 == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]



-- Ezercise 2

-- returns a list of fibonacci numbers 
-- takes less than a century to get the 100th number
fibs2 :: [Integer]
fibs2 = map fst fibs2Tuples


-- list of tuples, where fst is current fib in sequence, and snd is nezt fib in sequence
fibs2Tuples :: [(Integer, Integer)]
fibs2Tuples = iterate (\(prev2, prev1) -> (prev1, prev2 + prev1)) (0, 1)
  
testFib2 :: Bool          
testFib2 = take 15 fibs2 == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]



-- Ezercise 3

-- definition of stream. Basically an infinite list 
data Stream a = Cons a (Stream a)


-- converts a list to a stream 
streamToList :: Stream a -> [a]
streamToList (z `Cons` zs) = z : streamToList zs


-- Show a stream as its first 20 elements followed by "..."
instance Show a => Show (Stream a) where
    show = (++ "...") . filter (\z -> z /= '[' && z /= ']') . show . take 10 . streamToList 

testStream :: Stream Integer
testStream = 1 `Cons` testStream

testShowStream :: Bool
testShowStream = show testStream == "1,1,1,1,1,1,1,1,1,1..."


-- Ezercise 4
streamRepeat :: a -> Stream a
streamRepeat z = z `Cons` streamRepeat z

testStreamRepeat :: Bool
testStreamRepeat =  show testStream == show (streamRepeat 1 :: Stream Integer)


streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (z `Cons` zs) = f z `Cons` streamMap f zs

testStreamMap :: Bool
testStreamMap = take 10 (streamToList (streamMap (+ 1) testStream)) == [2,2,2,2,2,2,2,2,2,2]


streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f z = z `Cons` streamFromSeed f (f z) 

testStreamFromSeed :: Bool
testStreamFromSeed = take 10 (streamToList (streamFromSeed (+ 1) 0) :: [Integer]) == ([0,1,2,3,4,5,6,7,8,9] :: [Integer])


-- Ezercise 5 

-- List of natural numbers 
nats :: Stream Integer 
nats = streamFromSeed (+ 1) 0

-- returns highest power of 2 each natural number is divisible by 
-- 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4...
ruler :: Stream Integer
ruler = getRuler streamsOfNats

-- recursively builds ruler, given a streamsOfNats
getRuler :: Stream (Stream Integer) -> Stream Integer
getRuler ((num `Cons` strm) `Cons` strms) = num `Cons` interleaveStreams strm (getRuler strms)

-- 0,0,0,...,1,1,1,...,2,2,2,...,n,n,n,...
streamsOfNats :: Stream (Stream Integer)
streamsOfNats = streamMap streamRepeat nats

-- every other item is from first stream, every other item is from second stream. Starts with item from second Stream
-- interleaveStreams 1,2,3,4... a,b,c,d... == a,1,b,2,c,3,d,4...
interleaveStreams :: Stream a -> Stream a -> Stream a 
interleaveStreams (z `Cons` zs) (y `Cons` ys) = y `Cons` (z `Cons` interleaveStreams zs ys)

testRuler :: Bool
testRuler = take 16 (streamToList ruler) == [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4]


--Ezercise 6

x :: Stream Integer
x = 0 `Cons` (1 `Cons` streamRepeat 0)

instance Num (Stream Integer) where 
    fromInteger i = i `Cons` streamRepeat 0
    negate (z `Cons` strm) = ((-1) * z) `Cons` negate strm
    (+) (y `Cons` strmY) (z `Cons` strmZ) = (y + z) `Cons` (strmY + strmZ)
    (*) (y `Cons` strmY) zs@(z `Cons` strmZ) = (y * z) `Cons` (streamMap (y *) strmZ + (strmY * zs))

instance Fractional (Stream Integer) where
    (/) ys@(y `Cons` strmY) zs@(z `Cons` strmZ) = (y `div` z) `Cons` streamMap (`div` z) (strmY - strmZ * (ys / zs))

-- fibonacci defined using a generating function 
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ (2 :: Integer))

testFibs3 :: Bool
testFibs3 = take 10 (streamToList fibs3) == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
-- returns true if all tests return true
runTests :: Bool
runTests = testFib && 
           testFib1 && 
           testFib2 && 
           testShowStream && 
           testStreamRepeat && 
           testStreamMap && 
           testStreamFromSeed &&
           testRuler