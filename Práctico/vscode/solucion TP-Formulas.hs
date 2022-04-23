{- 
Autores: 

Lopez, Agustin
Pesce, Santiago
Vieyra, Mateo

-}


import qualified Data.Map as Map
import Data.List as List

data Formula = VAR Char | OR Formula Formula | AND Formula Formula | EQUIV Formula Formula | NOT Formula deriving (Eq, Show)

type Asign = Map.Map Char Bool

-- Dada un formula, devuelve la lista de variables en esa formula
vars :: Formula -> [Char]
vars (VAR x) = [x]
vars (NOT x) = vars x
vars (OR x y) = List.nub (vars x ++ vars y)
vars (AND x y) =  List.nub (vars x ++ vars y)
vars (EQUIV x y) =  List.nub (vars x ++ vars y)

-- Dadas una formula y una asignacion dice si la formula es verdadera o no
eval :: Formula -> Asign -> Bool
eval (VAR x) asign = asign Map.! x
eval (NOT x) asign = not (eval x asign)
eval (OR x y) asign = (eval x asign) || (eval y asign)
eval (AND x y) asign = (eval x asign) && (eval y asign)
eval (EQUIV x y) asign = (eval x asign) == (eval y asign)

--Dado una lista de variables proposicionales, devuelve una lista de listas, en la que cada lista contiene los posibles estados que toma la variable. Ej: f "pq" = [[('p', True),('p', False)], [('q', True),('q', False)]]
f :: [Char] -> [[(Char,Bool)]]
f [] = []
f (x:xs) =  [(x,y) | y<-[True,False]] : f xs

-- Dada una lista de variables devuelve la lista de todas las asignaciones
asigns :: [Char] -> [Asign]
asigns props = map Map.fromList (sequence (f props))

-- Dada una formula dice si es satisfacible
sat :: Formula -> Bool
sat form = any (eval form) (asigns (vars form))

-- Dada una formula dice si esa formula es una tautologıa o no
valid :: Formula -> Bool
valid form = all (eval form) (asigns (vars form))

-- Ejemplos de axiomas para probar con la funcion valid:

-- (p ∧ q ≡ p) ≡ (q ≡ p ∨ q)
reglaDorada :: Formula
reglaDorada = EQUIV (EQUIV (AND (VAR 'p') (VAR 'q')) (VAR 'p')) (EQUIV (VAR 'q') (OR (VAR 'p') (VAR 'q'))) 

-- p ∨ q ≡ q ∨ p
conmutativaDisyuncion :: Formula
conmutativaDisyuncion = EQUIV (OR (VAR 'p') (VAR 'q')) (OR (VAR 'q') (VAR 'p'))

-- p ∨ ¬p
terceroExcluido :: Formula
terceroExcluido = OR (VAR 'p') (NOT (VAR 'p'))

-- ¬¬p ≡ p
dobleNegacion :: Formula
dobleNegacion = EQUIV (NOT (NOT (VAR 'p'))) (VAR 'p') 

-- p ∧ p ≡ p
idempotenciaConj :: Formula
idempotenciaConj = EQUIV (AND (VAR 'p') (VAR 'p')) (VAR 'p')

-- p ∨ p ≡ p
idempotenciaDisy :: Formula
idempotenciaDisy = EQUIV (OR (VAR 'p') (VAR 'p')) (VAR 'p')

--Dice si una formula es una conjuncion
esConj :: Formula -> Bool
esConj (AND x y) = True 
esConj _ = False

--Dice si una formula es una negacion
esNeg :: Formula -> Bool
esNeg (NOT x) = True
esNeg _ = False

--Dice si una formula esta en DNF o no
esDNF :: Formula -> Bool
esDNF (OR x y)  | (esConj x || esNeg x) && (esConj y || esNeg y) = True
                | otherwise = False 
esDNF _ = False


