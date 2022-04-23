hd:: [a] -> a
hd (x:xs) = x

lasti:: [a] -> a
lasti [a] = a
lasti (x:xs) = lasti xs

inicial :: [a] -> [a]
inicial [x] = []
inicial (x:xs) = x : inicial xs

-- ejercicio 3
-- conc devuelve la concatenacion de dos listas

--[] lista vacia
--[x] lista de un elemento
--(x:xs) listas con al menos un elemento
--xs cualquier lista
--(_:xs) no importa cual sea el primer elemento de la lista

conc :: [a]->[a]->[a]
--conc [] [] = []
conc [] ys = ys
--conc xs [] = xs
conc (x:xs) ys = x: (conc xs ys)

--[1,2] [3,4]
--1: conc [2] [3,4]
--1: 2: conc [] [3,4]

cortar :: Int -> [a] -> [a]
cortar n [] = []
cortar 0 xs = xs
cortar n (x:xs) = cortar (n-1) xs

tomar:: Int -> [a] -> [a]
tomar 0 xs = []
tomar n [] = []
tomar n (x:xs) = x : (tomar (n-1) xs)

--concatenar de atras
concFinal :: [a] -> a -> [a]
concFinal [] y = [y]
concFinal (x : xs) y = x : concFinal xs y

-- cantDiv :: acumulador -> n -> iterador -> retorno
cantDiv :: Int -> Int -> Int 
cantDiv x 1 = 1
cantDiv x y | mod x y == 0 = 1 + cantDiv x (y-1)
            | otherwise = cantDiv x (y-1)

esPrimo :: Int -> Bool
esPrimo n = cantDiv n n == 2

-- primo 6 = cantDiv 6 6 == 2
--entro por linea 49
-- 1 + cantDiv 6 5 
--     cantDiv 6 4
--     cantDiv 6 3
-- 1 + cantDiv 6 2
-- 1 + cantDiv 6 1
-- 1


--Ejercicio 7 
primos :: Int -> [Int]
primos 3 = [2]
primos n | esPrimo (n-1) = (n-1) : primos (n-1)
         | otherwise = primos (n-1)

--revisar