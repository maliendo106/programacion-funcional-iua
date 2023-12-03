-- | En este módulo deben implementarse un conjunto de funciones utilizando
-- alguno de los siguientes mecanismos:
--
--   * Folds (@foldl@, @foldr@, @foldl'@, @foldl1@, @foldr1@)
--   * Listas por comprensión
--   * Composición de funciones ya definidas
module Lib (
    -- * Identificación del práctico
    name,
    -- * Funciones a implementar
    all', and', any', concat', elem', filter', find', length', map', maximum', minimum', notElem', or', product', reverse', sum', sumcubes, sumpowers, sumrange, sumsquares) where


-- | 'name' identifica al trabajo práctico
name :: String
name = "TP05"

-- Implemente todas las funciones usando foldl, foldr, foldl', foldl1, foldr1, listas por comprensión o una composición de funciones ya definidas.

-- | 'length'' devuelve la longitud de una lista finita como un entero (Int)
--
-- Ejemplos:
--
-- >>> length' "hola"
-- 4
-- >>> length' [1..10]
-- 10
-- >>> length' []
-- 0
length' :: [a] -> Int
length' = foldr (\_ x -> x+1) 0

-- | 'sum'' calcula la suma de una lista finita de números
--
-- Ejemplos:
--
-- >>> sum' [1..1000]
-- 500500
-- >>> sum' [1000,999..1]
-- 500500
-- >>> sum' []
-- 0
sum' :: Num a => [a] -> a
sum' = foldl (+) 0

-- | 'product'' calcula el producto de una lista finita de números
--
-- Ejemplos:
--
-- >>> product' [1..10]
-- 3628800
-- >>> product' [10,9..1]
-- 3628800
-- >>> product' []
-- 1
product' :: (Num a) => [a] -> a
product' = foldl (*) 1

-- | 'reverse'' toma una lista y la invierte
--
-- Ejemplos:
--
-- >>> reverse' "Hola"
-- "aloH"
-- >>> reverse' []
-- []
-- >>> reverse' [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
-- >>> reverse' (reverse' [1..10])
-- [1,2,3,4,5,6,7,8,9,10]
--
-- prop> \xs -> xs == reverse' (reverse' xs)
-- +++ OK, passed 100 tests.
reverse' :: [a] -> [a]
-- reverse' = error "No implementado"
reverse' = foldl (\acc x -> x:acc) []

-- | 'maximum'' devuelve el máximo de una lista de elementos
--
-- Si la lista está vacía, produce el error "empty list"
--
-- Ejemplos:
--
-- >>> maximum' "Hola"
-- 'o'
-- >>> maximum' [1..10]
-- 10
-- >>> maximum' [1,3,5,0,9,7,4,2,8,6]
-- 9
-- >>> maximum' [-1,-3,0,-5]
-- 0
-- >>> maximum' []
-- ... empty list
-- ...
maximum' :: Ord a => [a] -> a
maximum' = foldl1 (\x y -> if x > y then x else y)

-- | 'minimum'' devuelve el minimo de una lista de elementos
--
-- Si la lista está vacía, produce el error "empty list"
--
-- Ejemplos:
--
-- >>> minimum' "Hola"
-- 'H'
-- >>> minimum' [1..10]
-- 1
-- >>> minimum' [1,3,5,0,9,7,4,2,8,6]
-- 0
-- >>> minimum' [-1,-3,0,-5]
-- -5
-- >>> minimum' []
-- ... empty list
-- ...
minimum' :: Ord a => [a] -> a
-- minimum' = error "No implementado"
minimum' = foldl1 (\x y -> if x < y then x else y)

-- | 'map'' toma una función y una lista y aplica esa función a cada elemento
-- de la lista.
--
-- Devuelve una nueva lista.
--
-- Ejemplos:
--
-- >>> map' odd [1..10]
-- [True,False,True,False,True,False,True,False,True,False]
-- >>> map' (2*) [1..5]
-- [2,4,6,8,10]
-- >>> map' length ["uno", "dos", "tres", "cuatro"]
-- [3,3,4,6]
map' :: (a -> b) -> [a] -> [b]
-- map' = error "No implementado"
map' f xs = foldr (\x y -> f x : y) [] xs

-- | 'filter'' toma un predicado y una lista, y devuelve una nueva lista con
-- los elementos para los cuales se cumple el predicado.
--
-- Ejemplos:
--
-- >>> filter' odd [1..10]
-- [1,3,5,7,9]
-- >>> filter' (> 5) [6,5,1,8,9,0,3]
-- [6,8,9]
-- >>> filter' (< 0) [1..10]
-- []
filter' :: (a -> Bool) -> [a] -> [a]
-- filter' = error "No implementado"
filter' f xs = [x | x <- xs, f x]

-- | 'concat'' concatena una lista de listas
--
-- Ejemplos:
--
-- >>> concat' [[1,2,3],[1],[],[5,0]]
-- [1,2,3,1,5,0]
-- >>> concat' ["Hola",", ","Haskell"]
-- "Hola, Haskell"
concat'           :: [[a]] -> [a]
concat' = foldr (++) []

-- | 'and'' devuelve la conjunción de una lista de booleanos.
-- Para que el resultado sea True, la lista debe ser finita.
-- Para que sea False, debe haber un valor False en una posición finita
-- de una lista finita o infinita.
--
-- Ejemplos:
--
-- >>> and' [True, True, False]
-- False
-- >>> and' (replicate 100 True)
-- True
-- >>> and' (map (/=1000) [1..])
-- False
and' :: [Bool] -> Bool
and' = foldr (&&) True

-- | 'or'' devuelve la disyunción de una lista de booleanos.
-- Para que el resultado sea False, la lista debe ser finita.
-- Para que sea True, debe haber un valor True en una posición finita
-- de una lista finita o infinita.
--
-- Ejemplos:
--
-- >>> or' [True, True, False]
-- True
-- >>> or' (replicate 100 False)
-- False
-- >>> or' (map (==1000) [1..])
-- True
or' :: [Bool] -> Bool
-- or' = error "No implementado"
or' = foldr (||) False

-- | 'any'' toma un predicado y una lista y devuelve verdadero si el predicado
-- se cumple para algún elemento de la lista.
--
-- Ejemplos:
--
-- >>> any' odd [1..]
-- True
-- >>> any' odd [2,4..100]
-- False
-- >>> any' null ["uno", "dos", "tres", "cuatro"]
-- False
-- >>> any' even []
-- False
-- >>> any' (>1000) [1..]
-- True
any' :: (a -> Bool) -> [a] -> Bool
-- any' = error "No implementado"
any' p = or' . map' p

-- | 'all'' toma un predicado y una lista y devuelve verdadero si el predicado
-- se cumple para todos los elementos de la lista.
--
-- Ejemplos:
--
-- >>> all' odd [1..]
-- False
-- >>> all' even [2,4..10]
-- True
-- >>> all' (not . null) ["uno", "dos", "tres", "cuatro"]
-- True
all' :: (a -> Bool) -> [a] -> Bool
-- all' = error "No implementado"
all' p = and' . map' p

-- | 'elem'' x xs devuelve verdadero si x está en xs y falso en caso contrario
--
-- Ejemplos:
--
-- >>> 'o' `elem'` "Hola"
-- True
-- >>> 3 `elem'` [1..10]
-- True
-- >>> 0 `elem'` [1..1000]
-- False
-- >>> 9999 `elem'` [1..]
-- True
-- >>> 'h' `elem'` "Hola"
-- False
elem' :: Eq a => a -> [a] -> Bool
-- elem' = error "No implementado"
elem' = any' . (==)

-- | 'notElem'' x xs devuelve falso si x está en xs y verdadero en caso
-- contrario
--
-- Ejemplos:
--
-- >>> 'o' `notElem'` "Hola"
-- False
-- >>> 3 `notElem'` [1..10]
-- False
-- >>> 0 `notElem'` [1..1000]
-- True
-- >>> 9999 `notElem'` [1..1000]
-- True
-- >>> 'h' `notElem'` "Hola"
-- True
--
-- prop> \x xs -> (x `elem'` xs) || (x `notElem'` xs)
-- +++ OK, passed 100 tests.
--
-- prop> \x xs -> not ((x `elem'` xs) && (x `notElem'` xs))
-- +++ OK, passed 100 tests.
notElem' :: Eq a => a -> [a] -> Bool
-- notElem' = error "No implementado"
notElem' x xs = not $ elem' x xs

-- | 'find'' recibe un predicado y una lista y devuelve el primer elemento de
-- la lista que cumple ese predicado, o Nothing en caso contrario.
--
-- Ejemplos:
--
-- >>> find' (>10) [1..5]
-- Nothing
-- >>> find' (>3) [1..5]
-- Just 4
find' :: (a -> Bool) -> [a] -> Maybe a
-- find' = error "No implementado"
find' p = foldr (\x y -> if p x then Just x else y) Nothing

-- | 'sumrange' calcula la suma de números contenidos en un rango,
-- ambos extremos incluídos.
--
-- Ejemplos
--
-- >>> sumrange 0 10
-- 55
-- >>> sumrange 3 5
-- 12
-- >>> sumrange 3 3
-- 3
-- >>> sumrange 5 2
-- 0
sumrange :: (Ord a, Integral a) => a -> a -> a
-- sumrange = error "No implementado"
sumrange x y = foldr (+) 0 [x | x <- [x..y]] 

-- | 'sumsquares' calcula la suma de los cuadrados de los números en un rango
--
-- Ejemplos:
--
-- >>> sumsquares 1 10
-- 385
-- >>> sumsquares 5 4
-- 0
sumsquares :: (Ord a, Integral a) => a -> a -> a
-- sumsquares = error "No implementado"
sumsquares x y = foldr (+) 0 [x^2 | x <- [x..y]]

-- | 'sumpowers' calcula la suma de la enésima potencia de los números en un rango
--
-- Ejemplos:
--
-- >>> sumpowers 1 10 2
-- 385
-- >>> sumpowers 1 10 0
-- 10
-- >>> sumpowers 0 10 1
-- 55
sumpowers :: (Ord a, Integral a) => a -> a -> a -> a
-- sumpowers = error "No implementado"
sumpowers x y n = foldr (+) 0 [x^n | x <- [x..y]]

-- | 'sumcubes' calcula la suma de los cubos de los números en un rango
--
-- Ejemplos:
--
-- >>> sumcubes 1 10
-- 3025
-- >>> sumcubes 0 0
-- 0
-- >>> sumcubes 3 4
-- 91
sumcubes :: (Ord a, Integral a) => a -> a -> a
-- sumcubes = error "No implementado"
sumcubes x y = foldr (+) 0 [x^3 | x <- [x..y]]


