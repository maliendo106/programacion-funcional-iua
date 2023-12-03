import           Lib
import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad                        (unless)
import           Data.List
import           System.IO.Unsafe                     (unsafePerformIO)
import           Test.HUnit
import           Test.QuickCheck

main = defaultMain tests


throwsError :: (NFData a) => a -> String -> Bool
throwsError expr s = unsafePerformIO $ do
  res <- try $ evaluate (force expr)
  case res of
    Left (ErrorCall msg) -> return $ s `isPrefixOf` msg
    Right _              -> return False -- no exception


tests = [
        testGroup "Name" [
          testCase "name" (name @?= "TP01")
        ],
        testGroup "Numbers" [
          testProperty "int max'" ((\x y -> max x y == max' x y):: Int -> Int -> Bool),
          testProperty "double max'" ((\x y -> max x y == max' x y):: Double -> Double -> Bool),
          testProperty "char max'" ((\x y -> max x y == max' x y):: Char -> Char -> Bool),
          testProperty "max3 a" ((\a b c -> a > b && a > c ==> max3 a b c == a):: Int -> Int -> Int -> Property),
          testProperty "max3 b" ((\a b c -> b > a && b > c ==> max3 a b c == b):: Int -> Int -> Int -> Property),
          testProperty "max3 c" ((\a b c -> c > b && c > a ==> max3 a b c == c):: Int -> Int -> Int -> Property),
          testProperty "sum2" ((\a b c -> sum2 a b c == a+b+c-min a (min b c)):: Int -> Int -> Int -> Bool),
          testProperty "factorial" (\n -> n >=0 ==> factorial n == product [1..n]),
          testCase "fibo" (map fibo [0..15] @?= [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]),
          testProperty "square" ((\x -> square x == x*x):: Int -> Bool),
          testProperty "pow" ((\x n -> let e = abs n in pow x e == x^e):: Int -> Int -> Bool),
          testProperty "pow negative exponent" ((\x n -> n /= 0 ==> let e = negate (abs n) in throwsError (pow x e) "Negative exponent"):: Int -> Int -> Property),
          testProperty "sumrange" ((\a b -> sumrange a b == sum [a..b]):: Int -> Int -> Bool),
          testProperty "sumsquares" ((\a b -> sumsquares a b == sum (map (\x -> x*x) [a..b])):: Int -> Int -> Bool),
          testProperty "sumcubes" ((\a b -> sumcubes a b == sum (map (\x -> x*x*x) [a..b])):: Int -> Int -> Bool),
          testProperty "sumpowers" ((\a b n -> n >= 0 ==> sumpowers a b n == sum (map (^ n) [a..b])):: Int -> Int -> Int -> Property),
          testProperty "sumpowers negative exponent" ((\a b n -> n /= 0 && b >= a ==> let e = negate (abs n) in throwsError (sumpowers a b e) "Negative exponent"):: Int -> Int -> Int -> Property),
          testProperty "quadraticroots a 0 0" (\a -> a /= 0 ==> quadraticroots a 0 0 == ((0.0,0.0),(0.0,0.0))),
          testProperty "quadraticroots 0 b c" (\b c -> throwsError (quadraticroots 0 b c) "bad equation"),
          testProperty "quadraticroots a b c" prop_quadratic
          ],
          testGroup "Bool" [
            testProperty "not'" (\x -> not' x == not x),
            testProperty "or'"  (\a b -> or' a b  == (a || b)),
            testProperty "xor'" (\a b -> xor' a b == (a /= b)),
            testProperty "and'" (\a b -> and' a b == (a && b))
          ]
        ]

sq :: (Double, Double) -> (Double, Double)
sq (a,b) = (a*a-b*b,2*a*b)

mul :: (Double, Double) -> Double -> (Double, Double)
mul (a,b) x = (a*x, b*x)

add (a1, a2) (b1, b2) (c1, c2) = (a1+b1+c1,a2+b2+c2)

eval a b c x = add (mul (sq x) a) (mul x b) (c, 0)

modulus (x, y) = sqrt(x*x + y*y)

zero x = modulus x < 1e6

prop_quadratic a b c = a /= 0 ==> zero z1 && zero z2
    where (x1, x2) = quadraticroots a b c
          z1 = eval a b c x1
          z2 = eval a b c x2
