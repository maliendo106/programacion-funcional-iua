import           Lib
import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Control.Exception
import           Control.Monad                        (unless)
import           Data.List
import           Test.HUnit
import           Test.HUnit.Approx
import           Test.QuickCheck                      hiding (Result)
import           Test.QuickCheck.Assertions

main = defaultMain tests


catchToBool assertion message = catch
    (do
        evaluate assertion
        return False
    )
    ((\e -> return (message `isInfixOf` show e))::SomeException -> IO Bool)

assertRaises assertion message= do
    x <- catchToBool assertion message
    unless x  (assertFailure "Exception not thrown")


tests = [
        testGroup "Name" [
            testCase "name" (name @?= "TP05")
        ],
        testGroup "Lists" [
          testProperty "length'" ((\xs -> length xs == length' xs)::[Int] -> Bool),
          testProperty "int sum'" ((\xs -> sum xs == sum' xs)::[Int] -> Bool),
          testProperty "int product'" ((\xs -> product xs == product' xs)::[Int] -> Bool),
          testCase "null length" (length' [] @?= 0),
          testCase "zero product" (product' [3.1,2.3,5.0, 0.0] @?= 0.0),
          testProperty "double sum'" ((\xs -> sum xs ~==? sum' xs)::[Double] -> Result),
          testProperty "double product'" ((\xs -> product xs ~==? product' xs)::[Double] -> Result),
          testCase "double sum case" (assertApproxEqual "double sum" 1e-6 4.6 (sum' [1.2,3.4])),
          testCase "double product case" (assertApproxEqual "double product" 1e-6 4.08 (product' [1.2,3.4])),
          testProperty "reverse'" ((\xs -> reverse xs == reverse' xs)::String -> Bool),
          testProperty "maximum'" ((\xs -> xs /= [] ==> maximum xs == maximum' xs)::[Int] -> Property),
          testProperty "minimum'" ((\xs -> xs /= [] ==> minimum xs == minimum' xs)::[Int] -> Property),
          testCase "maximum' empty list" (assertRaises (maximum' []::[Int]) "empty list"),
          testCase "minimum' empty list" (assertRaises (minimum' []::[Int]) "empty list"),
          testProperty "map' odd" ((\xs ->  map odd xs == map' odd xs)::[Int] -> Bool),
          testProperty "map' (*3)" ((\xs ->  map (*3) xs == map' (*3) xs)::[Int] -> Bool),
          testProperty "map' length" ((\xs ->  map length xs == map' length xs)::[String] -> Bool),
          testCase "map' infinite list" (take 10 (map odd [1..]) @?= take 10 (map' odd [1..])),
          testProperty "filter' odd" ((\xs ->  filter odd xs == filter' odd xs)::[Int] -> Bool),
          testProperty "filter' (>3)" ((\xs ->  filter (>3) xs == filter' (>3) xs)::[Int] -> Bool),
          testProperty "filter' length" ((\xs ->  filter ((>3).length) xs == filter' ((>3).length) xs)::[String] -> Bool),
          testCase "filter' infinite list" (take 10 (filter odd [1..]) @?= take 10 (filter' odd [1..])),
          testProperty "concat'" ((\xs -> concat xs == concat' xs)::[String] -> Bool),
          testCase "concat' infinite list" (take 10 (concat (repeat [1,2])) @?= take 10 (concat' (repeat [1,2]))),
          testProperty "and'" ((\xs -> and xs == and' xs)::[Bool] -> Bool),
          testProperty "or'" ((\xs -> or xs == or' xs)::[Bool] -> Bool),
          testCase "and' infinite list" (and' (map (/=9) [1..]) @?= False),
          testCase "or' infinite list" (or' (map (==11) [1..]) @?= True),
          testProperty "any' Bool" ((\xs -> or xs == any' id xs)::[Bool] -> Bool),
          testProperty "any' Int" ((\xs -> any odd xs == any' odd xs)::[Int] -> Bool),
          testProperty "any' Char" ((\xs -> let letter = (`elem`['a'..'z']) in any letter xs == any' letter xs)::String -> Bool),
          testProperty "any' infinite list" ((\x -> x > 0 ==> any' (==x) [1..])::Int -> Property),
          testProperty "all' Bool" ((\xs -> and xs == all' id xs)::[Bool] -> Bool),
          testProperty "all' Int" ((\xs -> all odd xs == all' odd xs)::[Int] -> Bool),
          testProperty "all' Char" ((\xs -> let letter = (`elem`['a'..'z']) in  all letter xs == all' letter xs)::String -> Bool),
          testProperty "all' infinite list" ((\x -> x > 0 ==> not (all' (/=x) [1..]))::Int -> Property),
          testProperty "elem' chars"((\c xs -> elem c xs  ==> elem' c xs)::Char -> String -> Property),
          testProperty "elem' chars"((\c xs -> notElem c xs  ==> not (elem' c xs))::Char -> String -> Property),
          testProperty "elem' infinite list" ((\x -> x > 0 ==> elem' x [1..])::Int -> Property),
          testProperty "notElem' chars"((\c xs -> elem c xs  ==> not(notElem' c xs))::Char -> String -> Property),
          testProperty "notElem' chars"((\c xs -> notElem c xs  ==> notElem' c xs)::Char -> String -> Property),
          testProperty "find' chars"((\c xs -> elem c xs  ==> find' (==c) xs == Just c)::Char -> String -> Property),
          testProperty "find' chars"((\c xs -> notElem c xs  ==> find' (==c) xs == Nothing)::Char -> String -> Property),
          testProperty "find' Int" ((\xs -> find odd xs == find' odd xs)::[Int] -> Bool),
          testProperty "find' infinite list" ((\x -> x >= 0 ==> find' (>x) [1..] == Just (x+1)):: Int -> Property)
          ],
        testGroup "Numbers" [
          testProperty "sumrange" ((\a b -> sumrange a b == sum [a..b]):: Int -> Int -> Bool),
          testProperty "sumsquares" ((\a b -> sumsquares a b == sum (map (\x -> x*x) [a..b])):: Int -> Int -> Bool),
          testProperty "sumcubes" ((\a b -> sumcubes a b == sum (map (\x -> x*x*x) [a..b])):: Int -> Int -> Bool),
          testProperty "sumpowers" ((\a b n -> n >= 0 ==> sumpowers a b n == sum (map (^ n) [a..b])):: Int -> Int -> Int -> Property)
          ]
        ]

