--Implementación de distintos algoritmos de ordenamiento, todas las funciones xxxSort toman una lista de enteros y retorna
--la lista ordenada de manera ascendente (de menor a mayor).

insertionSort:: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insertionSort' (x:xs) []

insertionSort':: [Int] -> [Int] -> [Int]
insertionSort' [] ys = ys
insertionSort' (x:xs) ys = insertionSort' xs (insercionOrdenada ys x)

insercionOrdenada:: [Int] -> Int -> [Int]
--Dada una lista (ordenada de manera ascendente) y un entero,
--retornar la lista original con el entero insertado de manera ordenada


selectionSort:: [Int] -> [Int]
--completar usando separarMinimoYResto


separarMinimoYResto:: [Int] -> (Int, [Int])
--toma una lista y retorna una tupla con el mínimo y el resto de la lista

bruteSort:: [Int] -> [Int]
bruteSort [] = []
bruteSort xs = filtrarLaOrdenada todas
           where todas = permutaciones xs


filtrarLaOrdenada:: [[Int]] -> [Int]
--Dada una lista de listas de enteros (con al menos una ordenada de menor a mayor), retorna la ordenada

ordenada:: [Int] -> Bool
---Dada una lista de enteros retorna true si y solo si la lista está ordenada de manera ascendente

permutaciones:: [a] -> [[a]]
--Dada una lista de elementos retorna todas las posibles permutaciones de la misma