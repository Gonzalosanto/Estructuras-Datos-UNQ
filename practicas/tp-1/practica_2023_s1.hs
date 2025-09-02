-- https://github.com/Gonzalosanto/EstrD2023s1
-- Autor: gonzalosanto

sucesor :: Int -> Int
sucesor x = x + 1

sumar :: Int -> Int -> Int
sumar x y = x + y

divisionYResto :: Int -> Int -> (Int, Int)
-- Precond: el segundo elemento no puede ser 0
divisionYResto x 0 = error "No se puede dividir por cero"
divisionYResto x y = (div x y, mod x y)

maxDelPar :: (Int, Int) -> Int
maxDelPar (x, y) =
  if x > y
    then x
    else y

data Dir = Norte | Este | Sur | Oeste deriving (Show)

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving (Show)

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales x y = False

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte

--  DiaDeSemana ejercicio  --
primerYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primerYUltimoDia = (primerDia, ultimoDia)

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

{-vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes _ = False
vieneDespues Martes Lunes = True
vieneDespues Miercoles Lunes = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Lunes = True
vieneDespues Jueves Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Lunes = True
vieneDespues Viernes Martes = True
vieneDespues Viernes Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Sabado = False
vieneDespues Sabado Domingo = False
vieneDespues Sabado _ = True
vieneDespues Domingo _ = True-}

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes _ = False
vieneDespues _ Lunes = True
vieneDespues _ Martes = True
vieneDespues Miercoles d = empiezaConM d
vieneDespues Jueves d = empiezaConM d
vieneDespues Viernes d = noEsFinde d
vieneDespues Sabado d = noEsFinde d
vieneDespues Domingo _ = True
vieneDespues x y = noSonMismosDias x y

noEsFinde :: DiaDeSemana -> Bool
noEsFinde Sabado = False
noEsFinde Domingo = False
noEsFinde _ = True

noSonMismosDias :: DiaDeSemana -> DiaDeSemana -> Bool
noSonMismosDias Lunes Lunes = False
noSonMismosDias Martes Martes = False
noSonMismosDias Miercoles Miercoles = False
noSonMismosDias Jueves Jueves = False
noSonMismosDias Viernes Viernes = False
noSonMismosDias Sabado Sabado = False
noSonMismosDias _ _ = True

{-Bools tipo algebraico enumerativo-}
negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True x = x
implica False _ = True 

{-yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien x y = False

oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien x y = True-}

yTambien :: Bool -> Bool -> Bool
yTambien True x = x
yTambien False _ = False

oBien :: Bool -> Bool -> Bool
oBien False x = x
oBien True _ = True

{-Tipo algebraico => Registros-}
data Persona = P String Int deriving (Show)

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e + 1)

crecer' :: Persona -> Persona
crecer' p = P (nombre p) (sucesor (edad p))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre x (P n e) = P x e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P n e) (P nom ed) = e > ed

esMayorQueLaOtra' :: Persona -> Persona -> Bool
esMayorQueLaOtra' p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P n e) (P nom ed) =
  if e > ed
    then P n e
    else P nom ed

laQueEsMayor' :: Persona -> Persona -> Persona
laQueEsMayor' p1 p2 =
  if esMayorQueLaOtra p1 p2
    then p1
    else p2

data Pokemon = Pok TipoDePokemon Int deriving (Show)

data TipoDePokemon = Agua | Fuego | Planta deriving (Show)

data Entrenador = E String Pokemon Pokemon deriving (Show)

superaA :: Pokemon -> Pokemon -> Bool
superaA (Pok Agua _) (Pok Fuego _) = True
superaA (Pok Fuego _) (Pok Planta _) = True
superaA (Pok Planta _) (Pok Agua _) = True
superaA x y = False

superaA' :: Pokemon -> Pokemon -> Bool
superaA' pk1 pk2 = tipoSuperaA (tipo pk1) (tipo pk2)

tipoSuperaA :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSuperaA Agua Fuego = True
tipoSuperaA Fuego Planta = True
tipoSuperaA Planta Agua = True
tipoSuperaA _ _ = False

tipo :: Pokemon -> TipoDePokemon
tipo (Pok t _) = t

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe x ent = cantidadDeMismoTipo x (pokemon1De ent) (pokemon2De ent)

cantidadDeMismoTipo :: TipoDePokemon -> Pokemon -> Pokemon -> Int
cantidadDeMismoTipo t p1 p2 = unoSiEsDeMismoTipo t p1 + unoSiEsDeMismoTipo t p2

-- Variante con PM
esDeMismoTipo :: TipoDePokemon -> Pokemon -> Int
esDeMismoTipo Agua (Pok Agua _) = 1
esDeMismoTipo Fuego (Pok Fuego _) = 1
esDeMismoTipo Planta (Pok Planta _) = 1
esDeMismoTipo x y = 0

cantidadDePokemonDe' :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe' x ent = unoSiEsDeMismoTipo x (pokemon1De ent) + unoSiEsDeMismoTipo x (pokemon2De ent)

pokemon1De :: Entrenador -> Pokemon
pokemon1De (E _ pok _) = pok

pokemon2De :: Entrenador -> Pokemon
pokemon2De (E _ _ pok) = pok

unoSiEsDeMismoTipo :: TipoDePokemon -> Pokemon -> Int
unoSiEsDeMismoTipo t pok = if esDeMismoTipo' t (tipo pok) then 1 else 0

esDeMismoTipo' :: TipoDePokemon -> TipoDePokemon -> Bool
esDeMismoTipo' Agua Agua = True
esDeMismoTipo' Fuego Fuego = True
esDeMismoTipo' Planta Planta = True
esDeMismoTipo' _ _ = False

-- Variante con Eq
{-esPokemonDe :: TipoDePokemon -> Pokemon -> Int
esPokemonDe x (Pok y _) =
  if x == y
    then 1
    else 0-}

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (E _ pok1 pok2, E _ pok3 pok4) = [pok1, pok2, pok3, pok4]

juntarPokemon' :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon' (e1, e2) = unirPokemones (pokemonesDe e1) (pokemonesDe e2)

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe ent = [pokemon1De ent, pokemon2De ent]

unirPokemones :: [Pokemon] -> [Pokemon] -> [Pokemon]
unirPokemones pks1 pks2 = pks1 ++ pks2

{-Funciones Polimorficas-}
loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete x = 7

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

{-Funciones polimorficas sobre Listas con PM-}
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero (x : _) = x
elPrimero _ = error "La lista no puede estar vacia"

sinElPrimero :: [a] -> [a]
sinElPrimero (_ : xs) = xs
sinElPrimero _ = error "La lista no puede estar vacia"

splitHead :: [a] -> (a, [a])
splitHead (x : xs) = (x, xs)
splitHead _ = error "La lista no puede estar vacia"

splitHead' :: [a] -> (a, [a])
splitHead' xs = (elPrimero xs, sinElPrimero xs)
splitHead' _ = error "La lista no puede estar vacia"