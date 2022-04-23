import qualified Data.Map as M

data Formula = VAR Char | OR Formula Formula |AND Formula Formula | EQUIV Formula Formula | NOT Formula deriving (Eq, Show)

type Asign = M.Map Char Bool

--Formula = OR 'p' 'q'
--Asign = [('p' False), ('q' False)]

--M.fromlist [('p', True), ('q', True)]

--dada un formula, devuelve la lista de variables en esa formula
--vars :: Formula -> [Char]
--vars (Var 'p') =   

--dadas una formula y una asignacion dice si la formula es verdadera o no para esa asignacion
--eval :: Formula -> Asign -> Bool

idempotencia :: Formula
idempotencia = EQUIV (AND (VAR 'p') (VAR 'p')) (VAR 'p')
--p v p = p

--dada una lista de variables devuelve la lista de todas las asignaciones posibles para esas variables
--asigns :: [Char] -> [Asign]


--dada una formula dice si es satisfacible
--sat :: Formula -> Bool


--dada una formula dice si esa formula es una tautologıa o no
--valid :: Formula -> Bool


--dice si una formula esta en DNF o no
--esDNF :: Formula -> Bool
--esDNF form 

--esDis :: Formula -> Bool
--esDis form = if form == (OR x y) then True else False

esDNF :: Formula -> Bool
esDNF (OR x y) | x == (VAR x) && y == (VAR y) = True
               | otherwise = False
esDNF _ = False