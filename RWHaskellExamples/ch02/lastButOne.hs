-- file lastButOne.hs
lastButOne :: [a] -> a
lastButOne (x:y:[])  = x
lastButOne (x:xs)    = lastButOne xs

main = putStr("")
