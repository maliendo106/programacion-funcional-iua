-- | Módulo que implementa funciones utilizando recursividad de cola
module Lib (
    -- * Identificación del práctico
    name,
    -- * Funciones recursivas
    --
    -- | Todas las funciones siguientes deben ser implementadas utilizando
    -- recursividad de cola.
    factorial, fibo, length', maximum', minimum', pow, product', reverse', sum', sumcubes, sumpowers, sumrange, sumsquares) where

-- | 'name' identifica al trabajo práctico
name :: String
name = "TP03"


-- Implemente todas las funciones usando recursividad de cola

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
length' xs = len xs 0 
    where len [] acc = acc
          len (_:xs) acc = len xs (1 + acc)

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
sum' xs = suma xs 0
    where suma [] acc = acc
          suma (x:xs) acc = suma xs (x + acc)

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
product' xs = producto xs 1
    where producto [] acc = acc
          producto (x:xs) acc = producto xs (x * acc)

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
reverse' xs = rev xs []
    where 
        rev [] acc = acc
        rev (x:xs) acc = rev xs (x : acc)

-- reverse Hola -> rev Hola H -> rev ola Ho 

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
maximum' :: Ord a => [a] -> a
maximum' [] = error "empty list"
maximum' (x:xs) = trec xs x
    where 
        trec [] acc = acc
        trec (x:xs) acc
            | x >= acc = trec xs x 
            | otherwise = trec xs acc 

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
minimum' :: Ord a => [a] -> a
minimum' [] = error "empty list"
minimum' (x:xs) = trec xs x 
    where 
        trec [] acc = acc 
        trec (x:xs) acc
            | x < acc = trec xs x 
            | otherwise = trec xs acc

-- | 'factorial' calcula el factorial de un número entero (Integer)
--
-- Ejemplos:
--
-- >>> factorial 0
-- 1
-- >>> factorial 1
-- 1
-- >>> factorial 10
-- 3628800
-- >>> factorial 50
-- 30414093201713378043612608166064768844377641568960512000000000000
factorial :: Integer -> Integer
factorial n = fact n 1
    where 
        fact 0 acc = acc
        fact n acc = fact (n - 1) (n * acc)

-- | 'fibo' calcula el enésimo número de Fibonacci F(n) donde F(n) = F(n-1) + F(n-2)
--
-- Ejemplos:
--
-- >>> fibo 0
-- 0
-- >>> fibo 1
-- 1
-- >>> fibo 10
-- 55
-- >>> fibo 42
-- 267914296
fibo:: Integer -> Integer
fibo n = fib n 0 1
    where 
        fib 0 a _ = a
        fib n a b = fib (n-1) b (a+b)

-- fib 3 0 1 -> fib 2 1 1 -> fib 1 1 2 -> fib 0 2 3 -> 2 

-- | 'pow' x eleva un entero a una potencia entera
--
-- Ejemplos:
--
-- >>> pow 12 2
-- 144
-- >>> pow 2 10
-- 1024
-- >>> pow 3 0
-- 1
pow :: Integral a => a -> a -> a
pow x n = pw x n 1
    where
        pw _ 0 acc = acc
        pw x n acc = pw x (n-1) (acc*x)

-- pow 2 3 -> pw 2 3 1 -> pw 2 2 2 -> pw 2 1 4 -> pw 2 0 8 -> 8
-- pow 3 2 -> pw 3 2 1 -> pw 3 1 3 -> pw 3 0 9 -> 9

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
sumrange a b = sr a b 0
    where
        sr a b acc 
            | a == b = (b+acc)
            | a > b = 0
            | otherwise = sr (a+1) b (a+acc)     
-- sumrange 3 5 -> sr 3 5 0 -> sr 4 5 3 -> sr 5 5 7 -> 12 

-- | 'sumsquares' calcula la suma de los cuadrados de los números en un rango
--
-- Ejemplos:
--
-- >>> sumsquares 1 10
-- 385
-- >>> sumsquares 5 4
-- 0
sumsquares :: (Ord a, Integral a) => a -> a -> a
sumsquares a b = sq a b 0
    where
        sq a b acc
            | a == b = acc + (b*b)
            | a > b = 0
            | otherwise = sq (a+1) b (acc + (a*a))

-- sumsquares 3 5 -> sq 3 5 0 -> sq 4 5 (0+3*3) -> sq 5 5 (9+4*4) -> 25+5*5

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
sumpowers a b n = spo a b n 0
    where
        spo a b n acc
            | a == b = acc + (b ^ n)
            | a > b = 0
            | otherwise = spo (a+1) b n (acc + (a ^ n)) 

-- sumpowers 1 3 2 -> sp 1 3 2 x -> sp 2 3 2 (1*1)

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
sumcubes a b = sc a b 0
    where
        sc a b acc 
            | a == b = acc + (b*b*b)
            | a > b = 0 
            | otherwise = sc (a+1) b (acc + (a*a*a))
