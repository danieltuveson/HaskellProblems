module Hw3.Golf where 

-- import Safe (tailSafe)
import Data.List
-- skips :: [a] -> [[a]]
-- skips [] = []
-- skips [x] = [[x]]
-- skips l@(_:xs) = l : skips (second l)

-- Excercise 1 Hopscotch
skips :: [a] -> [[a]]
skips ls = map (`getNth` indLs) [1..length ls]
    where indLs = giveIndecies ls 

-- takes a list, returns the same list but with indecies starting from 1
-- giveIndecies "abc" == [(1, 'a'), (2, 'b'), (3, 'c')]
giveIndecies :: [a] -> [(Int, a)]
giveIndecies ls = zip [1..length ls] ls

-- given an int and a list, returns elements in a list whose index mod n == 0
getNth :: Int -> [(Int, a)] -> [a]
getNth n = map snd . filter (\(i, _) -> (i `mod` n) == 0) 


-- Exercise 2 Local maxima

localMaxima :: [Int] -> [Int]
localMaxima (x : y : z : l) = [y | x < y && z < y] ++ localMaxima (y : z : l)
localMaxima _ = []


--Exercise 3 Histogram

-- takes the result of makeHistogram and turns it into a string with the correct formatting
histogram :: [Int] -> String --(++ "\n") . intercalate "\n" . 
histogram =  (++ "\n") . intercalate "\n" . reverse . transpose . countsAsStrings . getCounts


-- takes in a list of counts of the form [2,3,...,4,...], returns a list of the form ["0=**  ", "1=*** ", ..., max="****", ...]
countsAsStrings :: [Int] -> [String]
countsAsStrings ls = map (countAsString ls) [0..9]


-- given a list of counts and an index, generates line of a histogram of the form "1=*** "
countAsString :: [Int] -> Int -> String
countAsString ls i = show i ++ "=" ++ replicate (ls !! i) '*' ++ replicate (maximum ls - (ls !! i)) ' '


-- returns the count of each number 0..9 in the input list. E.g. getCounts [1,2,2,7,7,7,9] == [0,1,2,0,0,0,0,3,0,1] 
getCounts :: [Int] -> [Int]
getCounts ls = map (`count` ls) [0..9]


-- given a thing and a list, counts the number of times thing appears in the list E.g. count 5 [1,5,2,3,4,5] == 2
count :: Eq a => a -> [a] -> Int 
count n = length . filter (== n)





















