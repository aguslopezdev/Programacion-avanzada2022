{- 

TRABAJO PRACTICO PROGRAMACION AVANZADA - 2021

Autores: 

Lopez, Agustin
Pesce, Santiago
Vieyra, Mateo

-}

import qualified Data.Map as Map
import Data.List as List (nub)

data Formula = VAR Char | OR Formula Formula | AND Formula Formula | EQUIV Formula Formula | NOT Formula deriving (Eq, Show)

type Asign = Map.Map Char Bool

-- Dada un formula, devuelve la lista de variables en esa formula. Se utiliza List.nub para eliminar las letras proposicionales repetidas.
vars :: Formula -> [Char]
vars (VAR x) = [x]
vars (NOT x) = vars x
vars (OR x y) = List.nub (vars x ++ vars y)
vars (AND x y) = List.nub (vars x ++ vars y)
vars (EQUIV x y) = List.nub (vars x ++ vars y)

-- Dadas una formula y una asignacion dice si la formula es verdadera o no
eval :: Formula -> Asign -> Bool
eval (VAR x) as = as Map.! x
eval (NOT x) as = not (eval x as)
eval (OR x y) as = (eval x as) || (eval y as)
eval (AND x y) as = (eval x as) && (eval y as)
eval (EQUIV x y) as = (eval x as) == (eval y as)

--Dado una lista de variables proposicionales, devuelve una lista de listas, en la que cada lista contiene los posibles estados que toma la variable. Ej: f "pq" = [[('p', True),('p', False)], [('q', True),('q', False)]]
f :: [Char] -> [[(Char,Bool)]]
f [] = []
f (x:xs) =  [(x,y) | y<-[True,False ]] : f xs

-- Dada una lista de variables devuelve la lista de todas las asignaciones posibles. La funcion sequence toma la lista de listas que retorna f props, y las combina, luego a cada combinacion se le aplica la funcion Map.fromList utilizando un map para obtener las asignaciones
asigns :: [Char] -> [Asign]
asigns props = map Map.fromList (sequence (f props))

-- Dada una formula dice si es satisfacible
sat :: Formula -> Bool
sat form = any (eval form) (asigns (vars form))

-- Dada una formula dice si esa formula es una tautologıa o no
valid :: Formula -> Bool
valid form = all (eval form) (asigns (vars form))

--any y all toman un predicado y una lista y comprueban si el predicado se satisface para algún o para todos los elementos respectivamente. Usamos estas funciones en lugar de tener que mapear un lista y luego usar and o or, lo cual seria:
--sat form = or $ map (eval form) $ asigns (vars form)
--valid form = and $ map (eval form) $ asigns (vars form)


-- Ejemplos de axiomas para probar con la funcion valid:

-- (p ∧ q ≡ p) ≡ (q ≡ p ∨ q)
reglaDorada :: Formula
reglaDorada = EQUIV (EQUIV (AND (VAR 'p') (VAR 'q')) (VAR 'p')) (EQUIV (VAR 'q') (OR (VAR 'p') (VAR 'q')))
-- valid reglaDorada retorna True

-- p ∨ q ≡ q ∨ p
conmutativaDisyuncion :: Formula
conmutativaDisyuncion = EQUIV (OR (VAR 'p') (VAR 'q')) (OR (VAR 'q') (VAR 'p'))
-- valid conmutativaDisyuncion retorna True

-- p ∨ ¬p
terceroExcluido :: Formula
terceroExcluido = OR (VAR 'p') (NOT (VAR 'p'))
-- valid terceroExcluido retorna True

-- ¬¬p ≡ p
dobleNegacion :: Formula
dobleNegacion = EQUIV (NOT (NOT (VAR 'p'))) (VAR 'p') 
-- valid dobleNegacion retorna True

-- p ∧ p ≡ p
idempotenciaConj :: Formula
idempotenciaConj = EQUIV (AND (VAR 'p') (VAR 'p')) (VAR 'p')
-- valid idempotenciaConj retorna True

-- p ∨ p ≡ p
idempotenciaDisy :: Formula
idempotenciaDisy = EQUIV (OR (VAR 'p') (VAR 'p')) (VAR 'p')
-- valid idempotenciaDisy retorna True

--Dice si una formula es una conjuncion. Utilizada como auxiliar en la funcion esDNF
esConj :: Formula -> Bool
esConj (AND x y) = True 
esConj _ = False

--Dice si una formula es una negacion. Utilizada como auxiliar en la funcion esDNF
esNeg :: Formula -> Bool
esNeg (NOT x) = True
esNeg _ = False
--Dice si una formula es una variable sola. Utilizada como auxiliar en la funcion esDNF
esVar :: Formula -> Bool
esVar (VAR x) = True
esVar _ = False

--Dice si una formula esta en DNF o no
esDNF :: Formula -> Bool
esDNF (OR x y)  | (esConj x || esNeg x || esVar) && (esConj y || esNeg y || esVar) = True
                | otherwise = False 
esDNF _ = False
