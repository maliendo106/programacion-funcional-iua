-- | Módulo que implementa una serie de funciones de orden superior
module Lib (
    -- * Identificación del práctico
    name,
    -- * Funciones de orden superior
    --
    -- | Todas las funciones que siguen reciben una función como argumento
    all', any', dropWhile', filter', iterate', map', partition', span', takeWhile', zipWith') where

-- | 'name' identifica al trabajo práctico
name :: String
name = "TP04"

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
-- map' f xs = error "No implementado"
-- map' f xs = mp f xs []
--     where
--         mp f [] acc = reverse acc
--         mp f (x:xs) acc = mp f xs (f x : acc) 
map' f [] = []
map' f (x:xs) = (f x) : map' f xs

-- map (2*) [1,2,3] = mp f xs [] -> mp f x:xs [] -> mp f 2,3 [1] -> mp f 3 [4,1]

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
filter' p [] = []
filter' p (x:xs)
    | p x = x : filter' p xs 
    | otherwise = filter' p xs

-- | 'zipWith'' toma una función que acepta dos parámetros, y dos listas
-- y devuelve una nueva lista que resulta unir las otras dos mediante la
-- función.
--
-- Ejemplos:
--
-- >>> zipWith' (+) [1..10] [10,9..1]
-- [11,11,11,11,11,11,11,11,11,11]
-- >>> zipWith' (*) [1..10] [1..]
-- [1,4,9,16,25,36,49,64,81,100]
-- >>> zipWith' (++) ["a", "b", "c"] ["1", "2", "3", "4"]
-- ["a1","b2","c3"]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith' f xs ys = error "No implementado"
zipWith' f [] ys = []
zipWith' f xs [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

-- | 'takeWhile'' toma un predicado y una función y devuelve el prefijo más
-- largo de la lista para el cual se verifica el predicado.
--
-- Ejemplos:
--
-- >>> takeWhile' (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile' (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile' (< 0) [1,2,3]
-- []
takeWhile' :: (a -> Bool) -> [a] -> [a]
-- takeWhile' p xs = error "No implementado"
-- takeWhile' p xs = tw p xs []
--     where 
--         tw _ [] acc = acc
--         tw p (x:xs) acc
--             | p x = tw p xs (acc ++ [x])
--             | otherwise = acc

takeWhile' p [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

-- | 'dropWhile'' toma un predicado y una función y descarta el prefijo más
-- largo de la lista para el cual se verifica el predicado.
--
-- Ejemplos:
--
-- >>> dropWhile' (< 3) [1,2,3,4,5,1,2,3]
-- [3,4,5,1,2,3]
-- >>> dropWhile' (< 9) [1,2,3]
-- []
-- >>> dropWhile' (< 0) [1,2,3]
-- [1,2,3]
dropWhile' :: (a -> Bool) -> [a] -> [a]
-- dropWhile' p xs = error "No implementado"
dropWhile' p [] = []
dropWhile' p (x:xs)
    | p x = dropWhile' p xs
    | otherwise = (x:xs)

-- | 'span'', toma un predicado p y una lista xs, y devuelve una tupla en la que
-- el primer elemento contiene los primeros elementos de xs que satisfacen p,
-- y el segundo elemento contiene el resto de la lista.
--
-- Ejemplos:
--
-- >>> span' (< 3) [1,2,3,4,1,2,3,4]
-- ([1,2],[3,4,1,2,3,4])
-- >>> span' (< 9) [1,2,3]
-- ([1,2,3],[])
-- >>> span' (< 0) [1,2,3]
-- ([],[1,2,3])
--
-- @span' p xs@ es equivalente a @(takeWhile p xs, dropWhile p xs)@
--
-- prop> \xs -> span' odd xs == (takeWhile odd xs, dropWhile odd xs)
-- +++ OK, passed 100 tests.
span' :: (a -> Bool) -> [a] -> ([a], [a])
-- span' p xs = error "No implementado"
span' p [] = ([], [])
span' p (x:xs)
    | p x       = (x : fst rest, snd rest)
    | otherwise = ([], x : xs)
  where
    rest = span' p xs

-- | 'partition'' toma un predicado y una lista y devuelve un par de listas
-- la primera de los cuales contiene los elementos que satisfacen el predicado
-- y la segunda los que no lo satisfacen.
--
-- Ejemplos:
--
-- >>> partition' odd [1..10]
-- ([1,3,5,7,9],[2,4,6,8,10])
-- >>> partition' (<5) [1..10]
-- ([1,2,3,4],[5,6,7,8,9,10])
-- >>> partition' (<'l') "Hola"
-- ("Ha","ol")
-- >>> partition' ((==6) . length) ["uno", "dos", "tres", "cuatro"]
-- (["cuatro"],["uno","dos","tres"])
--
-- @(partition' p xs)@ es equivalente a @(filter p xs, filter (not . p) xs)@
--
-- prop> \xs -> partition' odd xs == (filter odd xs, filter (not . odd) xs)
-- +++ OK, passed 100 tests.
partition' :: (a -> Bool) -> [a] -> ([a], [a])
-- partition' p xs = error "No implementado"
partition' p [] = ([],[])
partition' p (x:xs)
    | p x = (x : sx, sy)
    | otherwise = (sx, x : sy)
    where
        (sx, sy) = partition' p xs

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
-- any' p xs = error "No implementado"
any' p [] = False
any' p (x:xs)
    | p x = True
    | otherwise = any' p xs

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
-- all' p xs = error "No implementado"
all' p [] = True
all' p (x:xs)
    | p x = all' p xs
    | otherwise = False

-- | 'iterate'' toma una función y un valor inicial, y devuelve una lista infinita
-- que resulta de la aplicación repetida de la función sobre el valor inicial.
--
-- Ejemplos:
--
-- >>> take 10 (iterate' (*2) 1)
-- [1,2,4,8,16,32,64,128,256,512]
-- >>> take 5 (iterate' (\x -> x*x) 2)
-- [2,4,16,256,65536]
-- >>> take 5 (iterate' (\x -> succ(head x):x) "a")
-- ["a","ba","cba","dcba","edcba"]
iterate' :: (a -> a) -> a -> [a]
-- iterate' f x = error "No implementado"
iterate' f x = x : iterate' f (f x)



