--Ejercicio 1
data Nat = Zero | Suc Nat
    deriving Show

--Ejercicio 2
natToInt:: Nat -> Int
natToInt Zero = 0
natToInt (Suc n) = 1 + natToInt n

--Ejercicio 3
intToNat:: Int -> Nat
intToNat 0 = Zero
intToNat n = Suc (intToNat (n-1))

--Ejercicio 4
sumaNat:: Nat -> Nat -> Nat
sumaNat Zero n = n
sumaNat (Suc m) n = Suc(sumaNat m n)

--Ejercicio 5
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)
--data Tree a = Nil | Node {izquierdo::Tree a, raiz::a, derecho::Tree a} deriving(Show, Eq)


--Ejercicio 6
-- Devuelve el número de elementos en el árbol
-- numeroNodos (Tree)
numeroNodos :: (Num n) => Tree t -> n
numeroNodos Nil = 0
numeroNodos (Node _ izq der) = 1 + numeroNodos izq + numeroNodos der

--Ejercicio 7 
--Devuelve la altura del arbol
alturaArbol :: (Ord a) => Tree a -> Int
alturaArbol Nil = 0
alturaArbol (Node _ izq der) = 1 + max (alturaArbol izq) (alturaArbol der)

--Existe un elemento en el arbol
existe:: (Eq a) => a -> Tree a -> Bool
existe _ Nil = False
existe valor arbol | valor == r = True
                   | otherwise = (existe valor izq) || (existe valor der)
                where r = raiz arbol
                      izq = izquierdo arbol
                      der = derecho arbol

--Precondicion: el arbol es binario de búsqueda
existeB:: (Ord a) => a -> Tree a -> Bool
existeB _ Nil = False
existeB valor arbol | valor == r = True
                    | valor < r existeB valor izq
                    | otherwise = existeB valor der
                where r = raiz arbol
                      izq = izquierdo arbol
                      der = derecho arbol

--Nodo 104 (Nodo 71 (Nodo 17 (Nodo 3 Nil Nil) (Nodo 18 Nil Nil)) (Nodo 19 Nil Nil)) (Nodo 240 (Nodo 108 Nil (Nodo 110 Nil Nil)) (Nodo 245 Nil Nil))
