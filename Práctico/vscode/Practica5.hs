--genera lista infinita de unos
listInf :: [Int]
listInf = 1 : listInf

--devuelve una lista a partir de un numero dado
nLisInf :: Int -> [Int]
nLisInf n = n : nLisInf (n + 1)

--3. genera lista con los primeros n naturales
natLis :: Int -> [Int]
natLis n = [ x | x <- [1..n]]

--4. Retorna los primeros 5 elementos de una lista infinita de enteros positivos
--lista infinita
listInfinita :: Int -> [Int]
listInfinita x = x:listInfinita(x + 1)

cincoElem :: Int -> [Int]
cincoElem x = take x [1..x]

--5. Dada una lista de enteros, retornar sus cuadrados
cuadList :: [Int] -> [Int]
cuadList = map (^2)

cuadList2 :: [Int] -> [Int]
cuadList2 = map (\x -> x*x)

--6. Dado un entero positivo retornar sus divisores
--Definir divisores
divisible :: Int -> Int -> Bool
divisible x y = mod x y == 0

entDiv :: Int -> [Int]
entDiv x = filter (divisible x) [1..x `div` 2]

--7. Dada una lista de naturales obtener la lista que contenga
--solo los números primos de la lista original
cantDiv :: Int -> Int -> Int 
cantDiv x 1 = 1
cantDiv x y | mod x y == 0 = 1 + cantDiv x (y-1)
            | otherwise = cantDiv x (y-1)

esPrimo :: Int -> Bool
esPrimo n = cantDiv n n == 2

listPrimos :: [Int] -> [Int]
listPrimos = filter esPrimo

--es lo mismo
--divs n =  (==0).(n `mod`)
--div x y = mod x y == 0

--Ejercicio 8
sumaDeCuadrados :: [Int] -> Int
sumaDeCuadrados xs = sum (map (^ 2) xs)

sumCuad :: [Int] -> Int 
sumCuad xs =  foldr (+) 0 (map (^2) xs) 

sum2 :: [Int] -> Int 
sum2 n = foldl (+) 0 (map (^2) n)

sum3 :: [Int] -> Int 
sum3 n = foldl1 (+) (map (^2) n)

sum4 :: [Int] -> Int 
sum4 n = foldr1 (+) (map (^2) n)

--Ejercicio 9

listSuces:: [Int] -> [Int]
listSuces xs = map (+1) xs

--Ejercicio 11
facFoldL :: Int -> Int
facFoldL x = foldl (*) 1 [1..x]

--Ejercicio 12
listaSucc:: [Int] -> [Int]
listaSucc xs = [x+1| x <- xs]

--Ejercicio 13
listaCuad:: [Int] -> [Int]
listaCuad xs = [x^2|x<-xs]

--Ejercicio 14
listamayor8:: [Int]->[Int]
listamayor8 xs =[x | x<-xs, x `mod` 2==0, x>8]

--Ejercicio 15
prodCart:: [Int]->[Int]->(Int,Int)
prodCart xs ys = [(x,y)|x<-xs, y<-ys]

