-- module Set where 


data Set a = Set [a] 
           | Empty
           deriving(Show)


toSet :: [a] -> Set a 
