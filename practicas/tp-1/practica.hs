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

intercalar:: a -> [a] -> [a]
intercalar e [] = []
intercalar e [x] = [x]
intercalar e (x:xs) = x : e : intercalar e xs

snoc:: [a] -> a -> [a]
snoc [] e = [e]
snoc (x:xs) e = x : snoc xs e

append:: [a] -> [a] -> [a]
append [] ys = ys
append xs [] = xs
append (x:xs) ys = x : append xs ys

aplanar:: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = append x (aplanar xs)

reversa::[a] -> [a]
reversa [] = []
reversa (x:xs) = snoc (reversa xs) x

--OJO con el doble Pattern Matching
zipMaximos:: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos xs [] = xs
zipMaximos [] ys = ys
zipMaximos (x:xs) (y:ys) = (max x y) : zipMaximos xs ys

zipSort:: [Int] -> [Int] -> [(Int, Int)]
zipSort [] [] = []
zipSort [] _ = error "Longitudes de listas diferentes"
zipSort _ [] = error "Longitudes de listas diferentes"
zipSort (x:xs) (y:ys) = (min x y, max x y) : zipSort xs ys

-- RECURSION SOBRE NUMEROS --

factorial:: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva:: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n - 1)

contarHasta:: Int -> [Int]
contarHasta 0 = []
contarHasta n = snoc (contarHasta (n - 1)) n

replicarN:: Int -> a -> [a]
replicarN 0 x = []
replicarN n x = x : replicarN (n - 1) x

desdeHasta:: Int -> Int -> [Int]
desdeHasta n m
    | n > m = []
    | otherwise = n : desdeHasta (n + 1) m

takeN:: Int -> [a] -> [a]
takeN 0 _ = []
takeN _ [] = []
takeN n (x:xs) = x : takeN (n - 1) xs

dropN:: Int -> [a] -> [a]
dropN 0 xs = xs
dropN _ [] = []
dropN n (_:xs) = dropN (n - 1) xs

splitN:: Int -> [a] -> ([a],[a])
splitN 0 xs = ([], xs)
splitN _ [] = ([], [])
splitN n (x:xs) = (takeN n (x:xs), dropN n (x:xs))