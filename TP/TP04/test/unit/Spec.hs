import           Lib
import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Control.Exception
import           Control.Monad
import           Data.Char                            (isUpper, toUpper)
import           Data.List                            hiding (iterate')
import           Test.HUnit
import           Test.QuickCheck

main = defaultMain tests

tests = [
        testGroup "Name" [
                testCase "name" (name @?= "TP04")
        ],
        testGroup "Lists" [
          testProperty "map' odd" ((\xs ->  map odd xs == map' odd xs)::[Int] -> Bool),
          testProperty "map' (*3)" ((\xs ->  map (*3) xs == map' (*3) xs)::[Int] -> Bool),
          testProperty "map' length" ((\xs ->  map length xs == map' length xs)::[[Char]] -> Bool),
          testCase "map' infinite list" (take 10 (map odd [1..]) @?= take 10 (map' odd [1..])),
          testProperty "filter' odd" ((\xs ->  filter odd xs == filter' odd xs)::[Int] -> Bool),
          testProperty "filter' (>3)" ((\xs ->  filter (>3) xs == filter' (>3) xs)::[Int] -> Bool),
          testProperty "filter' length" ((\xs ->  filter ((>3).length) xs == filter' ((>3).length) xs)::[[Char]] -> Bool),
          testCase "filter' infinite list" (take 10 (filter odd [1..]) @?= take 10 (filter' odd [1..])),
          testProperty "zipWith' (+)" ((\xs ys -> zipWith (+) xs ys == zipWith' (+) xs ys)::[Int] -> [Int] -> Bool),
          testProperty "zipWith' (++)" ((\xs ys -> zipWith (++) xs ys == zipWith' (++) xs ys)::[[Char]] -> [[Char]] -> Bool),
          testProperty "zipWith' (==)" ((\xs ys -> zipWith (==) xs ys == zipWith' (==) xs ys)::[Int] -> [Int] -> Bool),
          testCase "zipWith' infinite list" (take 10 (zipWith (+) [1..] [1..]) @?= take 10 (zipWith' (+) [1..] [1..])),
          testProperty "takeWhile' odd" ((\xs ->  takeWhile odd xs == takeWhile' odd xs)::[Int] -> Bool),
          testProperty "takeWhile' (>0)" ((\xs ->  takeWhile (>0) xs == takeWhile' (>0) xs)::[Int] -> Bool),
          testProperty "takeWhile' length" ((\xs ->  takeWhile ((>3).length) xs == takeWhile' ((>3).length) xs)::[[Char]] -> Bool),
          testCase "takeWhile' infinite list" (take 10 (takeWhile (>0) [1..]) @?= take 10 (takeWhile' (>0) [1..])),
          testProperty "dropWhile' odd" ((\xs ->  dropWhile odd xs == dropWhile' odd xs)::[Int] -> Bool),
          testProperty "dropWhile' (>0)" ((\xs ->  dropWhile (>0) xs == dropWhile' (>0) xs)::[Int] -> Bool),
          testProperty "dropWhile' length" ((\xs ->  dropWhile ((>3).length) xs == dropWhile' ((>3).length) xs)::[[Char]] -> Bool),
          testProperty "span' odd" ((\xs ->  span odd xs == span' odd xs)::[Int] -> Bool),
          testProperty "span' (>0)" ((\xs ->  span (>0) xs == span' (>0) xs)::[Int] -> Bool),
          testProperty "span' length" ((\xs ->  span ((>3).length) xs == span' ((>3).length) xs)::[[Char]] -> Bool),
          testProperty "partition' odd" ((\xs ->  partition odd xs == partition' odd xs)::[Int] -> Bool),
          testProperty "partition' (>0)" ((\xs ->  partition (>0) xs == partition' (>0) xs)::[Int] -> Bool),
          testProperty "partition' length" ((\xs ->  partition ((>3).length) xs == partition' ((>3).length) xs)::[[Char]] -> Bool),
          testProperty "any' odd" ((\xs ->  any odd xs == any' odd xs)::[Int] -> Bool),
          testProperty "any' (>0)" ((\xs ->  any (>0) xs == any' (>0) xs)::[Int] -> Bool),
          testProperty "any' length" ((\xs ->  any ((>3).length) xs == any' ((>3).length) xs)::[[Char]] -> Bool),
          testCase "any' infinite list" (any (>0) [1..] @?= any' (>0) [1..]),
          testProperty "all' odd" ((\xs ->  all odd xs == all' odd xs)::[Int] -> Bool),
          testProperty "all' (>0)" ((\xs ->  all (>0) xs == all' (>0) xs)::[Int] -> Bool),
          testProperty "all' length" ((\xs ->  all ((>3).length) xs == all' ((>3).length) xs)::[[Char]] -> Bool),
          testCase "all' infinite list" (all (==0) [1..] @?= all' (==0) [1..]),
          testProperty "iterate' (*(-2))" ((\x -> take 10 (iterate (*(-2)) x) == take 10 (iterate' (*(-2)) x))::Int -> Bool),
          testProperty "iterate' (x:)" ((\x -> take 10 (iterate (x:) []) == take 10 (iterate' (x:) []))::Char -> Bool)
          ]
        ]

