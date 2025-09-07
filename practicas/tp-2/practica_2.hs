sumatoria:: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud:: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sucesores:: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = x+1 : sucesores xs

conjuncion:: [Bool] -> Bool
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

disyuncion:: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x && disyuncion xs

aplanar:: [[a]] -> [a]
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

pertenece:: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs

apariciones:: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = 
    if e==x  
        then 1 + apariciones e xs 
        else apariciones e xs

{- Forma de resolverlo usando guards ('|')
    | e == x = 1 + apariciones e xs
    | otherwise apariciones e xs
-}

losMenoresA:: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x:xs) = if n < x 
    then x : losMenoresA n xs
    else losMenoresA n xs

elMinimo:: Ord a => [a] -> a
elMinimo [] = error "No hay minimo en lista vacia"
elMinimo [x] = x
elMinimo (x:xs) = min x (elMinimo xs)

-- ...

-- REGISTROS

data Persona = P String Int deriving (Show)

edad:: Persona -> Int
edad (P _ e) = e

sumEdad:: [Persona] -> Int
sumEdad [] = 0
sumEdad (p:ps) = edad p + sumEdad ps

-- 1

mayoresA:: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (p:ps) = 
    if edad p > n
        then p : mayoresA n ps
        else mayoresA n ps

promedioEdad :: [Persona] -> Int
promedioEdad [] = error "Lista vacia" 
promedioEdad ps= div (sumEdad ps) (longitud ps) 

elMasViejo:: [Persona] -> Persona
elMasViejo [p] = p
elMasViejo (p:ps) = 
    if edad p > edad (elMasViejo ps)
        then p
        else elMasViejo ps

-- 3