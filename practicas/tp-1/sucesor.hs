sucesor:: Int -> Int
sucesor n = n + 1

sumar:: Int -> Int -> Int
sumar x y = x + y

maximo:: Int -> Int -> Int
maximo a b = if a > b 
    then a
    else b

--PUNTO 2
-- Pattern Matching

negar:: Bool->Bool
negar True = False
negar _ = True

andLogico::Bool -> Bool -> Bool
andLogico True True = True
andLogico _ _ = False

orLogico:: Bool -> Bool -> Bool
orLogico False False = False
orLogico _ _ = True

primera:: (Int,Int) -> Int
primera (x,_) = x

segunda:: (Int,Int) -> Int
segunda (_,y) = y

sumaPar :: (Int, Int) -> Int
sumaPar (a,b) = sumar a b

maxDelPar:: (Int, Int) -> Int
maxDelPar (x,y) = maximo x y

loMismo:: a -> a
loMismo x = x

siempreSiete:: a -> Int
siempreSiete _ = 7

duplicar:: a -> (a,a)
duplicar x = (x,x)

singleton :: a -> [a]
singleton x = [x]

isEmpty:: [a] -> Bool
isEmpty [] = True
isEmpty (_ : []) = False

head':: [a] -> a
head' [] = error "No se puede obtener el head de una lista vacia"
head' (x:_) = x

tail':: [a] -> [a]
tail' [] = error "No se puede obtener el tail de una lista vacia"
tail' (_:xs) = xs

--RECURSION
--PARTE BASE      funcion [] = elemento o caso base
--PARTE RECURSIVA funcion ls = elemento ... funcion (lista a recursar)
sumatoria::[Int]->Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs 

longitud::[a] -> Int
longitud [] = 0
longitud (a:as) = 1 + longitud as

promedio::[Int]->Int
promedio [] = 0 --OJO AL PIOJO no se puede dividir por 0
promedio ls = div (sumatoria ls) (longitud ls)

mapSucesor::[Int]->[Int]
mapSucesor [] = []
mapSucesor (x:xs) = (x+1) : mapSucesor xs

mapSumaPar::[(Int,Int)]->[Int]
mapSumaPar [] = [] 
mapSumaPar (t:ts) = (sumaPar t) : mapSumaPar ts

mapMaxDelPar::[(Int,Int)]->[Int]
mapMaxDelPar [] = []
mapMaxDelPar (t:ts) = maxDelPar t : mapMaxDelPar ts

todoVerdad::[Bool] -> Bool
todoVerdad [] = True
todoVerdad (b:bs) = b && todoVerdad bs

algunaVerdad::[Bool] -> Bool
algunaVerdad [] = False
algunaVerdad (b:bs) = b || algunaVerdad bs

pertenece:: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) =  (e == x) || pertenece e xs

apariciones::Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = if e == x 
    then 1 + apariciones e xs
    else 0 + apariciones e xs

filtrarMenoresA :: Int -> [Int]-> [Int]
filtrarMenoresA n [] = []
filtrarMenoresA n (x:xs) = if n < x 
    then x : filtrarMenoresA n xs
    else filtrarMenoresA n xs

filtrarElemento :: Eq a => a -> [a]-> [a]
filtrarElemento e [] = []
filtrarElemento e (x:xs) = if e == x 
    then filtrarElemento e xs
    else x : filtrarElemento e xs

mapLongitudes::[[a]] -> [Int]
mapLongitudes [] = []
mapLongitudes (ls:lss) = longitud ls : mapLongitudes lss

longitudMayorA :: Int -> [[a]] -> [[a]]
longitudMayorA n [] = []
longitudMayorA n (ls:lss) = if longitud ls > n 
    then ls : longitudMayorA n lss
    else longitudMayorA n lss