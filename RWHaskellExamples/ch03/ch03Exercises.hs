import Data.List (sortBy)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

len :: [a] -> Int
len [] = 0
len (_ : []) = 1
len (_ : xs) = len (xs) + 1

mySum :: [Int] -> Int
mySum [] = 0
mySum (x : []) = x
mySum (x : xs) = x + mySum xs

mean :: [Int] -> Double
mean x = fromIntegral((mySum x)) / fromIntegral((len x))

makePalindrome :: [a] -> [a]
makePalindrome [] = []
makePalindrome (x : xs) = x : (makePalindrome xs) ++ [x]

reverseLs :: [a] -> [a]
reverseLs [] = []
reverseLs (x : xs) = (reverseLs xs) ++ [x]

isPalindrome :: [Int] -> Bool
isPalindrome [] = True
isPalindrome (_ : []) = True
isPalindrome x = (head x) == (last x) && isPalindrome (tail (init x))


sortSubs :: [[a]] -> [[a]] 
sortSubs x = let lenCompare elt1 elt2 = compare (length elt1) (length elt2)
             in sortBy lenCompare x

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x : []) = x
intersperse sep (x : xs)  = x ++ [sep] ++ (intersperse sep xs)


height :: Tree a -> Int
height someTree = 
    case someTree of 
        Empty                -> 0
        (Node _ Empty Empty) -> 1
        (Node _ x y)         -> if hx >= hy
                                then 1 + hx
                                else 1 + hy
                                where hx = height x
                                      hy = height y

t0 = Empty                                                                                                               
t1 = Node "a" t0 t0
t2 = Node "b" t1 t1                                                                                                      
t3 = Node "c" t1 t2  
t4 = Node "d" Empty t2


data Direction = Left 
               | Right
               | Straight

data Point = Double Double 

-- getDirection :: (Point, Point, Point) -> Direction
-- getDirection _ = Straight
-- getDirection ((ax, ay), (bx, by), (cx, cy))




















