{- Haskell HW 1-}

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
    | n <= 0    = []
    | otherwise = (n `mod` 10) : toDigitsRev(n `div` 10)

reverseList :: [Integer] -> [Integer]
reverseList []       = []
reverseList (x : []) = [x]
reverseList (x : xs) = (reverseList xs) ++ [x]

doubleEveryOtherFwd :: [Integer] -> [Integer]
doubleEveryOtherFwd [] = []
doubleEveryOtherFwd (x : []) = [x]
doubleEveryOtherFwd (x : y : xs) = x : 2 * y : doubleEveryOtherFwd xs

doubleEveryOther :: [Integer] -> [Integer] 
doubleEveryOther ls = reverseList (doubleEveryOtherFwd (reverseList ls))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = x `div` 10 + x `mod` 10  + sumDigits xs

validate :: Integer -> Bool
validate x = sumDigits(doubleEveryOther(toDigits(x))) `mod` 10 == 0 

main :: IO ()
main = print(validate(4012888888881882))


