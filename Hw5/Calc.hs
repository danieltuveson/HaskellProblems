module Hw5.Calc where 

import Hw5.ExprT
import Hw5.Parser


-- Exercise 1:
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2


-- Exercise 2:
evalStr :: String -> Maybe Integer
evalStr str = 
    case expr of 
        Nothing -> Nothing
        Just m -> Just (eval m)
    where expr = parseExp Lit Add Mul str


-- Exercise 3
class Expr a where 
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit :: Integer -> ExprT
    add = Add :: ExprT -> ExprT -> ExprT
    mul = Mul :: ExprT -> ExprT -> ExprT


-- Exercise 4
instance Expr Integer where 
    lit i   = i 
    add i j = i + j 
    mul i j = i * j 

instance Expr Bool where 
    lit i
        | i <= 0    = False
        | otherwise = True
    add i j = i || j 
    mul i j = i && j 

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where 
    lit = MinMax 
    add (MinMax i) (MinMax j) = MinMax (max i j)
    mul (MinMax i) (MinMax j) = MinMax (min i j)

instance Expr Mod7 where 
    lit i   = Mod7 (i `mod` 7)
    add (Mod7 i) (Mod7 j) = Mod7 ((i + j) `mod` 7)
    mul (Mod7 i) (Mod7 j) = Mod7 ((i * j) `mod` 7)



    