merge:: [Int]->[Int]->[Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y then x:merge xs (y:ys) else y:merge (x:xs) ys

bubble:: [Int]->[Int]
bubble (x:y:xs) = if x>y
    then y:bubble (x:xs)
    else x:bubble(y:xs)
bubble xs = xs

bubbleSort:: [Int]->[Int]
bubbleSort [] = []
bubbleSort xs = bubbleSort (init(bubble xs)) ++ [last(bubble xs)]

bubbleSort' :: [Int]->[Int]
bubbleSort' [] = []
bubbleSort' xs = bubbleSort' (init tl) ++ [last tl]
    where
    tl = bubble xs

insercionOrdenada:: [Int]->Int->[Int]
insercionOrdenada [] a = [a]
insercionOrdenada (x:xs) a | a<=x = a:x:xs
    | otherwise = x:insercionOrdenada xs a

insertSort:: [Int]->[Int]
insertSort [] = []
insertSort (x:xs) = insercionOrdenada (insertSort xs) x

delete:: Int->[Int]->[Int]
delete n (x:xs) | n==x =xs
    | otherwise = x:delete n xs

selectSort :: [Int] -> [Int]
selectSort [] = []
selectSort xs = m : selectSort (delete m xs)
    where m = minimum xs

minimo :: [Int] -> Int
minimo [x] = x
minimo (x:y:xs) = minimo ((min x y) : xs)

menores :: Int -> [Int] -> [Int]
menores n [] = []
menores n (x:xs)
    | x < n = x : menores n xs
    | otherwise = menores n xs

mayores :: Int -> [Int] -> [Int]
mayores n [] = []
mayores n (x:xs)
    | x >= n = x : mayores n xs
    | otherwise = mayores n xs

qSort :: [Int] -> [Int]
qSort [] = []
qSort [x] = [x]
qSort (x:xs) = qSort men ++ [x] ++ qSort may
    where
    men = menores x xs
    may = mayores x xs -- mayores o iguales

-- Ejercicio 3

cuad :: Int -> Int
cuad 0 = 1
cuad n = 2 * cuad (n-1)

--Ejercicio 4

bin :: Int -> [Int]
bin 0 = [0]
bin 1 = [1]
bin n = (bin (n `div` 2)) ++ [n `mod` 2]

--Ejercicio 7
--dado 4 y s = [s,s,s,s]
--repetido :: Int -> 

--Ejercicio 8
nelem :: [a] -> Int -> a
nelem (x:xs) 0 = x
nelem (x:xs) n = nelem xs (n-1)

--Listas por comprension
--Ejercicio 6
cuadPerf :: Int -> Bool
cuadPerf n = [x | x <- [0..n], x*x == n] /= []