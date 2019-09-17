{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Hw5.CalcExercise5 where

import Hw5.StackVM
import Hw5.Calc (Expr(..))

-- Exercise 5 

-- data StackVal = IVal Integer | BVal Bool | Void deriving Show

-- -- The various expressions our VM understands.
-- data StackExp = PushI Integer
--               | PushB Bool
--               | Add
--               | Mul
--               | And
--               | Or
--                 deriving Show

-- type Stack   = [StackVal]
-- type Program = [StackExp]

-- -- Execute the given program. Returns either an error message or the
-- -- value on top of the stack after execution.
-- stackVM :: Program -> Either String StackVal
-- stackVM = execute []

instance Expr Program where 
    -- lit :: Integer -> Program
    lit _   = [Add]
    -- add :: Program -> Program -> Program
    add _ _ = [Add]
    -- mul :: Program -> Program -> Program
    mul _ _ = [Add]


