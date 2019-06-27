type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 src tgt aux  = []
hanoi 1 src tgt aux = [(src, tgt)]
hanoi n src tgt aux = (hanoi (n - 1) src aux tgt) ++ [(src, tgt)] ++ (hanoi (n - 1) aux tgt src)




main :: IO ()
main = print(hanoi 3 "a" "b" "c")

