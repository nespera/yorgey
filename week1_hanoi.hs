type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a) 

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 a b c d = [(a,b)]
hanoi4 2 a b c d = [(a,c), (a,b), (c,b)]
hanoi4 3 a b c d = [(a,c), (a,d), (a,b), (d,b), (c,a)]
hanoi4 n a b c d = (hanoi4 (n-2) a c b d) ++ [(a,d), (a,b), (d,b)] ++ (hanoi4 (n-2) c b a d) 
