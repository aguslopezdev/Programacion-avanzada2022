--Ejercicio 1
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)

sumaNodos:: Tree Int -> Int
sumaNodos Nil = 0
sumaNodos (Node r i d) = sumar r (sumaNodos i) (sumaNodos d)
    where sumar x y z = x + y + z

--DNI: 39967996
--Node 3 (Node 9 (Node 6 Nil Nil) Nil) (Node 9 Nil Nil)
numDoc:: Tree Int
numDoc = Node 3 (Node 9 (Node 6 Nil Nil) Nil) (Node 9 Nil Nil)

--Ejercicio 3
compact:: [Int] -> [Int]
compact [] = []
compact [x] = [x]
compact (x:y:xs)
      | x==y = (compact (y:xs))
      | otherwise = [x] ++ (compact (y:xs))

{-
compact [3,9,9,6,7,9,9,6]
[Def compact]
3==9 -> False
[Otherwise]
[3]++compact [9,9,6,7,9,9,6]
[Def compact]
9==9 -> True
[3]++[9,6,7,9,9,6]
[Def compact]
9==6 -> False
[Otherwise]
[3]++[9]++compact[6,7,9,9,6]
...
[3,9,6,7,9,6]
-}

--Ejercicio 4

{-
Orden aplicativo. Mas adentro e izquierda

(square inf) + (square inf)
={Def square}
inf*inf + (square inf)
={Def square}
(inf*inf) + (inf*inf)
={Def inf}
(inf+1*inf) + (inf*inf)
={Def inf}
(inf+1*inf+1) + (inf*inf)
={Def inf}
(inf+1*inf+1) + (inf+1*inf+1)
...
Entra en un ciclo infinito

Orden normal. Mas afuera e izquierda

(square inf) + (square inf)
={Def square}
inf*inf + (square inf)
={Def inf}
inf+1*inf + (square inf)
={Def inf}
inf+1+1*inf + (square inf)
...
Entra en ciclo infinito

--------------------------------------

and (inf == inf) ((square 2) == 4)

Orden aplicativo

={Def square}
and (inf == inf) (2*2 == 4)
={Def ==}
and (True) (2*2 == 4)
={Aritmetica}
and (True) (4 == 4)
={Def ==}
and (True) (True)
={Def and}
True

Orden normal

={Def ==}
and (True) ((square 2) == 4)
={Def and}
(square 2) == 4
={Def square}
2*2 == 4
={Aritmetica}
4 == 4
={Def ==}
True

-}