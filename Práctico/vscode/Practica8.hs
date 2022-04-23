--Ejercicio 1
nand:: Bool -> Bool -> Bool
nand True True = False
nand x y = False

nand2:: Bool -> Bool -> Bool
nand2 x y = length(filter (==True)[x,y]) /= 2

--Ejercicio 2
maj :: Bool -> Bool -> Bool -> Bool
maj True True False = True
maj True False True = True
maj False True True = True
maj True True True = True
maj x y z = False

--Ejercicio 3

--Existe al menos un elemento par en la lista

lispar :: [Int] -> Bool
lispar (x:xs) = or $ [even $ xs!!i | i <-[0..(length xs)-1]]

--Todos los elementos de la lista en posiciones impares son positivos

limpar :: [Int] -> Bool
limpar (x:xs) = and $ [xs!!i > 0 | i <- [0..(length xs)-1], i `mod` 2 == 1]

--Ejercicio 4

sumatoria :: [Int] -> Int
sumatoria xs = sum $ [xs!!i | i <- [0..(length xs)-1], mod i 2 == 1]

producto :: [Int] -> Int
producto xs = product $ [xs!!i | i <- [0..(length xs)-1], mod i 2 == 1]

contatoria :: [Int] -> Int
contatoria xs = length [xs!!i | i <- [0..(length xs)-1]] 

contatoria2 :: [a] -> Int
contatoria2 [] = 0
contatoria2 (x:xs) = 1 + contatoria2 xs

universal :: [a] -> (a -> Bool) -> Bool
universal xs p = and $ [p (xs!!i) | i <- [0..(length xs)-1]]

universal2 :: [a] -> (a -> Bool) -> Bool
universal2 [] _ = True
universal2 (x:xs) p = p x && universal2 xs p