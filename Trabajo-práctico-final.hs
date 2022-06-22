-- TP Prog. Avanzada 2022 <Laberinto> . Autores: Bernardi - Lopez.
import qualified Data.Map as M
  
  -- | El tipo laberinto:
data Laberinto = Lab { size :: Int                    -- | El tamaño del laberinto, un laberinto es NxN
                     , lindantes :: M.Map Pos [Pos]   -- | Los las posiciones lindantes a cada posicion 
                     , inicial :: Pos                 -- | La posicion inicial
                     , final :: Pos                   -- | La posicion final
                     } deriving (Eq)
----------------------------------------
{-Instance del show:
-}
instance Show (Laberinto) where 
  show (Lab s l i f) = techo s ++ "\n" ++ "|" ++ graficaLab (M.keys l) s l 

{-
| Dibuja techos segun el tamaño del laberinto
| AUX de instance Show 
-}
techo :: Int -> String
techo 0 = ""
techo n = " ___" ++ techo (n-1)

{-
| Funcion que grafica un laberinto dependiendo su tamaño y los lindantes de cada posicion
| AUX de instance Show 
-}
graficaLab :: [Pos] -> Int -> Diccionario -> String
graficaLab [] n d = " ESCAPA DEL LABERINTO!|"
graficaLab ((a,b):xs) n d  |(n-1)==b = paredyopiso (a,b) d ++ "\n|" ++ (graficaLab xs n d) -- Cuando la posicion actual llega al borde
                           |n>b = paredyopiso (a,b) d ++ (graficaLab xs n d) -- Cuando la posicion actual no es un borde del laberinto

{-
| Funcion que dibuja una pared y/o piso, segun una posicion sea o no lindante de: la posicion de su derecha y la posicion de abajo
| AUX de instance graficaLab
-}
paredyopiso :: Pos -> Diccionario -> String
paredyopiso (a,b) d | elem (a, b+1) ((d) M.! (a,b)) && elem (a+1, b) ((d) M.! (a,b)) = "    "            -- No hay pared ni suelo
                    | not(elem (a, b+1) ((d) M.! (a,b))) && not(elem (a+1, b) ((d) M.! (a,b))) = "___|"  -- Hay pared y no hay suelo
                    | not(elem (a, b+1) ((d) M.! (a,b))) && elem (a+1, b) ((d) M.! (a,b)) =  "   |"      -- Hay pared y no hay suelo
                    | elem (a, b+1) ((d) M.! (a,b)) && not(elem (a+1, b) ((d) M.! (a,b))) = "___ "       -- No hay pared y si hay suelo

----------------------------------------

-- | Un tipo para las posiciones, una posicion es una coordenada
type Pos = (Int, Int)

-- | Un camino es una secuencia de posiciones
type Camino = [Pos]

-- | Tipo diccionario
type Diccionario = M.Map Pos Camino

--Nuevo tipo para pasar de diccionario a lista
type ListaKyV = [((Int,Int),[(Int,Int)])]

---------------------------------------------------------------------------------------------------------------------
--Ejemplos de laberintos para pruebas
lab2 = Lab {
size = 3
-- Diccionario de lindantes de nuestro laberinto inventado para tests
 ,lindantes = M.fromList [((0,0),[]),
                          ((0,1),[]),
                          ((0,2),[(1,2)]), 
                          ((1,0),[(1,1),(2,0)]), 
                          ((1,1),[(1,0),(1,2)]), 
                          ((1,2),[(0,2),(1,1)]), 
                          ((2,0),[(1,0)]),
                          ((2,1),[]),
                          ((2,2),[])
                          ]
 ,inicial = (2,0)
 ,final = (0,2)
}

labInvalido = Lab {
size = 3
-- Diccionario de lindantes de nuestro laberinto INVALIDO inventado para tests
 ,lindantes = M.fromList [((0,0),[(0,1),(1,0)]),
                          ((0,1),[(0,2)]),
                          ((2,2),[(1,2)]), 
                          ((1,0),[(1,1),(2,1)]), 
                          ((1,1),[(1,0),(1,2)]), 
                          ((1,2),[(0,2),(1,1)]), 
                          ((2,0),[(1,0)]),
                          ((2,1),[(2,2)]),
                          ((2,2),[(2,3)]) -- Lindante invalido (2,3)
                          ]
 ,inicial = (2,0)
 ,final = (0,2)
}
---------------------------------------------------------------------------------------------------------------------

{-| 
    Dice si un laberinto es valido:
        - Tamaño positivo.
        - La entrada y salida estan en los bordes.
        - Todas las posiciones son correctas [0..N) con N el tamaño del laberinto.
        - Para cada posición, todas las posiciones lindantes válidas deben existir en el mapa.
|   FUNCION (1)
-}
esValido :: Laberinto -> Bool
esValido l = tamPositivo (size l) && (ensalBord (inicial l) (final l) (size l)) && (posCorrec l) && (lindantesExisten (kyv l))

{-
| Funcion que determina si el tamaño del laberinto es correcto (es un laberinto de tamaño positivo)
| AUX (1.1) de esValido
-}
tamPositivo :: Int -> Bool
tamPositivo n = n > 0

{-
| Funcion que determina si 2 posiciones (se usara para entrada y salida) estan en el borde del laberinto
| AUX (1.2) de esValido
-}
ensalBord :: Pos -> Pos -> Int -> Bool
ensalBord (a,b) (h,k) n = (esBorde (a,b) n) && (esBorde (h,k) n) 

{-
| Funcion que determina si una posicion es borde
| AUX (1.2.0) de ensalBord
-}
esBorde :: Pos -> Int -> Bool
esBorde (a,b) n = a==0 || b==0 || a==(n-1) || b==(n-1) --Si una coordenada es 0 o el tamaño menos 1, esta en el borde

{-
| Funcion que determina que todas las posiciones (keys) son correctas
| AUX (1.3) de esValido
| Descripcion: Primero compara que la longitud de las claves del diccionario sea igual al tamaño del laberinto al cuadrado ya que, la cantidad de claves necesarias para que el laberinto sea correcto debe ser gual a la cantidad de posiciones posibles,
luego, tambien, se debe comprobar que la primera clave arrojada al ser la minima posible ya que las claves se arrojan de manera ordenada, debe ser igual al par (0,0)
de manera analoga, ocurre lo mismo con la maxima posicion posible, siendo esta igual al par (n-1,n-1)
-}
posCorrec :: Laberinto -> Bool
posCorrec l = (length(M.keys(lindantes l)) == ((size l) * (size l))) && ((head(M.keys(lindantes l))) == (0,0)) && ((head(reverse(M.keys(lindantes l))))==(((size l)-1),((size l)-1)))

{-
| Funcion que determina, si los valores correspondientes a las claves son correctos, es decir, si PODRIAN ser lindantes de tal posicion en el laberinto
| AUX (1.4) de esValido
-}
lindantesExisten :: ListaKyV -> Bool
lindantesExisten [] = True
lindantesExisten (x:xs) | esEsquina (x:xs) = comparaEsquinas x n && lindantesExisten xs
                        | esBorde1 (x:xs) = comparaBordes x n  && lindantesExisten xs
                        | esCentro (x:xs) = comparaCentros x n && lindantesExisten xs
                         where n = (div (length (x:xs)) (length (x:xs))) -- n vendria a ser una "Raiz cuadrada entera" de la cantindad de claves en la lista (cant de claves en la lista= n*n)

{-
| Funcion que dado un laberinto devuelve una lista con sus respectivas claves y valores
| AUX (1.4.0) de lindantesExisten
-}
kyv :: Laberinto -> ListaKyV
kyv l = M.assocs (lindantes l)

{-
| Si la clave dada corresponde a una esquina, toma el valor de 2
| AUX (1.4.1.0) de lindantesExisten
-}
esEsquina :: ListaKyV -> Bool
esEsquina (x:xs) = esqBorCen (fst x) n == 0
  where n = (div (length (x:xs)) (length (x:xs)))

{-
| Si la clave dada corresponde a un borde, toma el valor de 2
| AUX (1.4.1.1) de lindantesExisten
-}
esBorde1 :: ListaKyV -> Bool
esBorde1 (x:xs) = esqBorCen (fst x) n == 1
  where n = (div (length (x:xs)) (length (x:xs)))

{- 
| Si la clave dada corresponde a un centro, toma el valor de 2
| AUX (1.4.1.2) de lindantesExisten
-}
esCentro :: ListaKyV -> Bool
esCentro (x:xs) = esqBorCen (fst x) n == 2
  where n = (div (length (x:xs)) (length (x:xs)))

{-
| Todas estas, (1.4.1.0, .1 y .2) llaman a la funcion que determina si es esq borde o centro con el primer elemento de la tupla, el cual viene a ser la clave
luego, lo compara con el valor que debe de tomar, n toma el valor especificado por lo aclarado en (1.4)
-}

{-| 
| Funcion que determina si una clave dada corresponde a la esquina al borde o al centro del laberinto, dependiendo de su tamaño(n)
| AUX (1.4.2) de esEsquina, esBorde1, esCentro
-}
esqBorCen :: Pos -> Int -> Int
esqBorCen (a,b) n | (a== 0 && b==0) || (a==0 && b==(n-1)) || (a==(n-1) && b==0) ||(a==(n-1) && b==(n-1)) = 0 --Esquina toma valor 0, para determinar que estas disyunciones son verdaderas se hizo un analisis aritmético
                  | esBorde (a,b) n = 1 --Borde toma el valor 1
                  | otherwise = 2 -- Centro toma valor 2

{-| 
| Funcion que dada una clave la cual es una pos. esquina y sus valores lindantes, determina si son correctos, es decir si los mismos existen dentro del laberinto, dependiendo de su tamaño (n)
| Todos los casos fueron analizados aritmeticamente y son correctos
| AUX (1.4.3.0) de lindantesExisten
-}
comparaEsquinas :: (Pos,Camino)-> Int-> Bool
comparaEsquinas ((f,c),[]) n = True
comparaEsquinas ((f,c),((a,b):xs)) n | f==0 && c==0     = ((a==(f+1) && b==c) || (a==f && b==(c+1))) && (comparaEsquinas((f,c), xs) n)
                                     | f==0 && c==(n-1) = ((a==(f+1) && b==c) || (a==f && b==(c-1))) && (comparaEsquinas((f,c), xs) n)
                                     | f==(n-1) && c==0 = ((a==(f-1) && b==c) || (a==f && b==(c+1))) && (comparaEsquinas((f,c), xs) n)
                                     | f==(n-1) && c==(n-1) = ((a==f && b==(c-1)) || (a==(f-1) && b==c)) && (comparaEsquinas((f,c), xs) n)
                                     | otherwise = False

{-| 
| Funcion que dada una clave la cual es una pos. borde y sus valores lindantes, determina si son correctos, es decir si los mismos existen dentro del laberinto, dependiendo de su tamaño (n)
| Todos los casos fueron analizados aritmeticamente y son correctos
| AUX (1.4.3.1) de lindantesExisten
-}
comparaBordes :: (Pos,Camino)-> Int-> Bool
comparaBordes ((f,c),[]) n = True
comparaBordes ((f,c),((a,b):xs)) n | f==0  = (a==f && b==(c-1)) || (a==(f+1) && b==c)|| (a==f && b==(c+1)) && (comparaBordes((f,c), xs) n)
                                   | f==(n-1) = (a==f && b==(c-1)) || (a==(f-1) && b==c)|| (a==f && b==(c+1)) && (comparaBordes((f,c), xs) n)
                                   | c==0  = ((a==(f-1) && b==c) || (a==(f+1) && b==c)) || (a==f && b==(c+1)) && (comparaBordes((f,c), xs) n)
                                   | c==(n-1)  = ((a==(f-1) && b==c) || (a==(f+1) && b==c)) || (a==f && b==(c-1)) && (comparaBordes((f,c), xs) n)
                                   | otherwise = False

{-| 
| Funcion que dada una clave la cual es una pos. centro y sus valores lindantes, determina si son correctos, es decir si los mismos existen dentro del laberinto, dependiendo de su tamaño (n)
| Todos los casos fueron analizados aritmeticamente y son correctos
| AUX (1.4.3.2) de lindantesExisten
-}
comparaCentros:: (Pos,Camino)-> Int-> Bool
comparaCentros ((f,c),[]) n = True
comparaCentros ((f,c),((a,b):xs)) n = ((a==(f-1) && b==c) || (a==f && b==(c-1)))|| (a==(f+1) && b==c)|| (a==f && b==(c+1)) && (comparaCentros((f,c), xs) n)


{-|
| Funcion que dadas dos posiciones en el laberinto, la conecta, si son lindantes y si existe pared entre ellas, actualizando el mapa del laberinto con los valores actualizados
| FUNCION (2)
-}
tirarPared :: Pos -> Pos -> Laberinto -> Laberinto
tirarPared (f,c) (a,b) l  | lindantesExisten[((f,c),[(a,b)])]= l {lindantes = (concaValores (f,c) (a,b) l) }
                          | otherwise = error"No es posible tirar una pared ya que la posiciones ingresadas no son lindantes"

{-
| Funcion que toma dos posiciones y un laberinto y devuelve el mapa de dicho laberinto actualizado, agregandole a una clave (f,c), un valor (a,b); y a otra clave (a,b), un valor (f,c)
| AUX (2.1) de tirarPared
-}
concaValores ::  Pos -> Pos -> Laberinto -> Diccionario
concaValores (f,c) (a,b) l = M.insertWith (++) (a,b) [(f,c)] (M.insertWith (++) (f,c) [(a,b)] (lindantes l))


{-
| Funcion que determina si una secuencia de posiciones lindantes que están en el laberinto 
| FUNCION (3) 
-}
esCamino :: Camino -> Laberinto -> Bool
esCamino [] l = True
esCamino [(a,b)] l = pertenece (a,b) l
esCamino ((a,b):(c,d):xs) l | pertenece (a,b) l = (esValorDe (a,b) (c,d) l) && (esCamino ((c,d):xs) l)
                            | otherwise = False

{-
| Funcion que dada una poscion y un laberinto determina si la misma pertenece a el
| AUX (3.1) de esCamino y de construirCamino
-}
pertenece :: Pos -> Laberinto -> Bool
pertenece (a,b) l = a>=0 && b>=0 && a<(size l) && b<(size l)

{-
| Funcion que dada una clave y un valor, determina si ese valor esta incluido dentro de los valores de dicha clave
| AUX (3.2) de esCamino
-}
esValorDe :: Pos -> Pos -> Laberinto -> Bool
esValorDe (a,b) (c,d) l = elem (c,d) ((lindantes l) M.! (a,b))


{-
| Funcion que dada una lista de posiciones y un laberinto, construye un camino en el
| FUNCION (4)
-}
construirCamino :: [Pos] -> Laberinto -> Laberinto
construirCamino [] l = l
construirCamino [(a,b)] l = if (pertenece (a,b) l) then l else error"Posicion invalida"
construirCamino ((a,b):(c,d):xs) l | esCamino ([(a,b),(c,d)]) l = construirCamino ((c,d):xs) l
                                   | (pertenece (a,b) l) = construirCamino ((c,d):xs) (tirarPared (a,b) (c,d) l)
                                   | otherwise = error"El camino tiene una posicion invalida"