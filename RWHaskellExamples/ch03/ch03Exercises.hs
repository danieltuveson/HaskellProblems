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


data Direction = L | R | S
               deriving(Show)

data Point = Point Double Double 


-- Stolen from wikipedia on Graham scan algorithm 
-- Takes cross product of vectors p1p2 and p1p3, determines rotation from that
getDirection :: Point -> Point -> Point -> Direction
getDirection (Point x1 y1) (Point x2 y2) (Point x3 y3) 
    | crossProduct > 0  = L 
    | crossProduct < 0  = R
    | crossProduct == 0 = S
    where crossProduct = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)



-- takes a list of points, and returns a list of directions 
getDirections :: [Point] -> [Direction]
getDirections (p1 : p2 : p3 : ps) = (getDirection p1 p2 p3) : (getDirections (p2 : p3 : ps))
getDirections _ = []


p0 = (Point (-2) (-8))                                                                                                                          
p1 = (Point (-1) (-1))                                                                                                                          
p2 = Point 0 0                                                                                                                               
p3 = Point 1 1 
p4 = Point 2 8                                                                                                                               
p5 = Point 3 27
pList = p0 : p1 : p2 : p3 : p4 : p5 : []  



-- -- Gets direction given 3 points
-- getDirection :: Point -> Point -> Point -> Direction
-- getDirection p1 p2 p3
--     | angle == truncPi = S
--     | angle < truncPi  = L
--     | angle > truncPi  = R
--     where angle = truncDouble (getAngle p1 p2 p3) numDigits
--           truncPi = truncDouble pi numDigits
--           numDigits = 8

-- -- truncates last up to last n digits. Stolen from stackoverflow:
-- -- https://stackoverflow.com/questions/18723381/rounding-to-specific-number-of-digits-in-haskell/31952975
-- truncDouble :: Double -> Int -> Double
-- truncDouble x n = (fromIntegral (floor (x * (10 ^ n)))) / 10 ^ n  


-- -- given three points, gives angle between them using law of cosines 
-- getAngle :: Point -> Point -> Point -> Double
-- getAngle p1 p2 p3 = let a = distance p1 p2
--                         b = distance p2 p3
--                         c = distance p1 p3
--                     in acos ((a ** 2.0 + b ** 2.0 - c ** 2.0) / (2 * a * b))
-- -- need to account for extra 180°

-- -- Takes in two points, returns distance between them
-- distance :: Point -> Point -> Double
-- distance (Point px py) (Point qx qy) = sqrt ((qy - py) ** 2.0 + (qx - px) ** 2.0) 


-- -- Defines a linear function based on two input points 
-- -- The resulting function takes in x and returns the y value for the line
-- -- Returns NaN if ay == by
-- line :: Point -> Point -> Double -> Double
-- line (Point ax ay) (Point cx cy) x = ((ay - cy) / (ax - cx)) * x + (ax * cy - ay * cx) / (ax - cx)


-- getDirection :: Point -> Point -> Point -> Direction
-- getDirection p q r =
--     | (x > 0) && (y > 0)
--     | (x > 0) && (y < 0)
--     | (x < 0) && (y > 0)
--     | (x < 0) && (y < 0)

--     where (Point px py) = p
--           (Point qx qy) = q
--           (Point rx ry) = r











