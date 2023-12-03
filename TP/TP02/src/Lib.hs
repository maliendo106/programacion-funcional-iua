-- | Módulo que implementa funciones básicas de procesamiento de listas
module Lib
    (
        -- * Identificación del práctico
        name,
        -- * Funciones de procesamiento de listas
        (+++), at, cycle', drop', elem', fst', head', init', last', length', maximum', minimum', null', product', repeat', replicate', reverse', snd', sum', tail', take', unzip', zip'    ) where

-- | 'name' identifica al trabajo práctico
name :: String
name = "TP02"

-- | 'head'' devuelve el primer elemento de una lista, que no puede ser vacia
--
-- Si la lista está vacía, produce el error "empty list"
-- Ejemplos:
--
-- >>> head' "hola"
-- 'h'
-- >>> head' [1..10]
-- 1
-- >>> head' []
-- *** Exception: empty list
-- ...
head' :: [a] -> a
head' [] = error "empty list"
head' (xs:_) = xs
--head' xs = xs !! 0

-- | 'tail'' devuelve todos los elementos de una lista no vacia excepto el primero
--
-- Si la lista está vacía, produce el error "empty list"
--
-- Ejemplos:
--
-- >>> tail' "hola"
-- "ola"
-- >>> tail' [1..10]
-- [2,3,4,5,6,7,8,9,10]
-- >>> tail' []
-- *** Exception: empty list
-- ...
tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (_:xs) = xs

-- | 'last'' toma una lista y devuelve su último elemento
--
-- Si la lista está vacía, produce el error "empty list"
--
-- Ejemplos:
--
-- >>> last' "hola"
-- 'a'
-- >>> last' [1..10]
-- 10
-- >>> last' []
-- *** Exception: empty list
-- ...
last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x 
last' (_:xs) = last' xs

-- | 'init'' toma una lista y devuelve todo excepto el último elemento
--
-- Si la lista está vacía, produce el error "empty list"
--
-- Ejemplos:
--
-- >>> init' "hola"
-- "hol"
-- >>> init' [1..10]
-- [1,2,3,4,5,6,7,8,9]
-- >>> init' []
-- *** Exception: empty list
-- ...
init' :: [a] -> [a]
init' [] = error "empty list"
init' [x] = []
init' (x:xs) = x : init' xs

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
length' [] = 0
length' xs = 1 + length' (tail xs)

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
sum' [] = 0
sum' (x:xs) = x + sum' xs 

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
product' [] = 1
product' (x:xs) = x * product' xs 

-- | 'null'' devuelve verdadero si una lista está vacía y falso en caso contrario
--
-- Ejemplos:
--
-- >>> null' "Hola"
-- False
-- >>> null' ""
-- True
-- >>> null' [1..]
-- False
-- >>> null []
-- True
null' :: [a] -> Bool
null' [] = True
null' _ = False

-- | (+++) concatena dos listas
--
-- Ejemplos:
--
-- >>> "Hola" +++ " mundo"
-- "Hola mundo"
-- >>> [1..10] +++ [10,9..1]
-- [1,2,3,4,5,6,7,8,9,10,10,9,8,7,6,5,4,3,2,1]
-- >>> [] +++ [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
-- >>> [1..10] +++ []
-- [1,2,3,4,5,6,7,8,9,10]
-- >>> [] +++ []
-- []
(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)

-- | 'at' xs n devuelve el enésimo elemento de la lista (se comporta como (!!))
--
-- Si n es negativo, produce el error "negative index".
--
-- Si el índice es mayor o igual al tamaño de la lista, produce el error "index too large"
--
-- Ejemplos:
--
-- >>> "Hola" `at` 1
-- 'o'
-- >>> [1..] `at` 1000
-- 1001
-- >>> "Hola" `at` 4
-- *** Exception: index too large
-- ...
-- >>> [1..] `at` (-1)
-- *** Exception: negative index
-- ...
at :: [a] -> Int -> a
at (x:xs) n
    | n < 0 = error "negative index"
    -- | n >= length (x:xs) = error "index too large" 
    | n == 0 = x
    | otherwise = at xs (n - 1)
at [] _ = error "index too large"

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
elem' e (x:xs) 
    | e == x = True
    | otherwise = elem' e xs
elem' e [] = False

-- | 'take'' n xs devuelve una lista con los primeros n elementos de xs
--
-- Ejemplos:
--
-- >>> take' 3 "Hola"
-- "Hol"
-- >>> take' 5 "Hola"
-- "Hola"
-- >>> take' 0 [1..10]
-- []
-- >>> take' 10 [1,3..]
-- [1,3,5,7,9,11,13,15,17,19]
take' :: Int -> [a] -> [a]
take' n xs
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

-- | 'drop'' n xs devuelve una lista en la que se han descartado los primeros
-- n elementos de xs
--
-- Ejemplos:
--
-- >>> drop' 3 "Hola"
-- "a"
-- >>> drop' 5 "Hola"
-- ""
-- >>> drop' 0 "Hola"
-- "Hola"
-- >>> drop' 10 []
-- []
-- >>> drop' 5 [1,3..15]
-- [11,13,15]
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
    | n <= 0 = (x:xs)
    | otherwise = drop' (n - 1) xs

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
reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = last xs : reverse' (take (length xs - 1) xs)

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
-- *** Exception: empty list
-- ...
mayor :: Ord a => a -> a -> a
mayor a b
    | a > b = a 
    | otherwise = b

maximum' :: Ord a => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = mayor x (maximum' xs)

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
-- *** Exception: empty list
-- ...
menor :: Ord a => a -> a -> a
menor a b
    | a < b = a 
    | otherwise = b

minimum' :: Ord a => [a] -> a
minimum' [] = error "empty list"
minimum' [x] = x
minimum' (x:xs) = menor x (minimum' xs)

-- | 'repeat'' toma un elemento y devuelve una lista infinita de ese elemento
--
-- Ejemplos:
--
-- >>> take 10 (repeat' 'x')
-- "xxxxxxxxxx"
-- >>> take 5 (repeat' 99)
-- [99,99,99,99,99]
-- >>> take 3 (repeat' [1,2])
-- [[1,2],[1,2],[1,2]]
-- >>> take 5 (repeat' [])
-- [[],[],[],[],[]]
-- >>> take 5 (repeat' "")
-- ["","","","",""]
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- | 'cycle'' toma una lista no vacía y la reproduce infinitamente
--
-- Si la lista está vacía, produce el error "empty list"
--
-- Ejemplos:
--
-- >>> take 10 (cycle' "Hola")
-- "HolaHolaHo"
-- >>> take 10 (cycle' [1,2])
-- [1,2,1,2,1,2,1,2,1,2]
-- >>> take 10 (cycle' [])
-- *** Exception: empty list
-- ...
cycle' :: [a] -> [a]
cycle' [] = error "empty list"
cycle' xs = xs ++ cycle' xs

-- | 'replicate' n x devuelve una lista con n copias de x
--
-- Ejemplos:
--
-- >>> replicate' 10 'a'
-- "aaaaaaaaaa"
-- >>> replicate' 10 10
-- [10,10,10,10,10,10,10,10,10,10]
-- >>> replicate' 5 []
-- [[],[],[],[],[]]
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n - 1) x

-- | 'fst'' devuelve el primer elemento de un par
--
-- Ejemplos:
--
-- >>> fst' (1,2)
-- 1
-- >>> fst' (True, 'a')
-- True
fst' :: (a, b) -> a
fst' (x, _) = x

-- | 'snd'' devuelve el segundo elemento de un par
--
-- Ejemplos:
--
-- >>> snd' (1,2)
-- 2
-- >>> snd' (True, 'a')
-- 'a'
snd' :: (a, b) -> b
snd' (_, x) = x 

-- | 'zip'' recibe dos listas y devuelve una lista de pares
--
-- La longitud de la lista resultante es la longitud de la lista de entrada
-- más corta.
--
-- Ejemplos:
--
-- >>> zip' ['a'..'f'] [1..]
-- [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6)]
-- >>> zip' [1..] []
-- []
-- >>> zip' [] [1..10]
-- []
-- >>> zip' "Hola" "Haskell"
-- [('H','H'),('o','a'),('l','s'),('a','k')]
zip' :: [a] -> [b] -> [(a, b)]
zip' xs [] = []
zip' [] ys = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys 

-- | 'unzip'' recibe una lista de pares y devuelve un par de listas
--
-- Ejemplos:
--
-- >>> unzip' [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6)]
-- ("abcdef",[1,2,3,4,5,6])
-- >>> unzip' []
-- ([],[])
-- >>> unzip' [('x',[])]
-- ("x",[[]])
-- >>> unzip' [('H','H'),('o','a'),('l','s'),('a','k')]
-- ("Hola","Hask")

ua ((a,b):xs) = [a] ++ ua xs
ua [] = []

ub ((a,b):xs) = [b] ++ ub xs
ub [] = []

unzip' :: [(a, b)] -> ([a], [b])
unzip' xs = (ua xs, ub xs)  
