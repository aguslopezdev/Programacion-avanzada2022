reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

iguales :: [a] -> [a] -> Bool
iguales [] [] = True
iguales (x:xs) (y:ys) = x == y  && iguales xs ys

