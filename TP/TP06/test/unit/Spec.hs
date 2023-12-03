import           Lib
import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Control.Exception
import           Control.Monad                        (unless)
import           Data.List                            hiding (foldl', foldl1',
                                                       iterate')
import           Data.Maybe                           (isJust, isNothing)
import           Test.HUnit
import           Test.QuickCheck

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


fhl :: [a] -> List a
fhl = foldr (:::) Empty

fl :: List a -> [a]
fl = foldr' (:) []

wrap :: (List a -> List b) -> ([a] -> [b])
wrap f = fl.f.fhl

xsCheck :: Eq b => ([a] -> b) -> (List a -> b) -> [a] -> Bool
xsCheck f g xs = f xs == (g.fhl) xs

xsWrap :: Eq b => ([a] -> [b]) -> (List a -> List b) -> [a] -> Bool
xsWrap f g xs = f xs == wrap g xs

xsysWrap :: Eq c => ([a] -> [b] -> [c]) -> (List a -> List b -> List c) -> [a] -> [b] -> Bool
xsysWrap f g xs ys = fhl (f xs ys) == g (fhl xs) (fhl ys)

t2p :: (a,b) -> Pair a b
t2p (a,b) = Pair a b

lt2lp :: [(a,b)] -> List (Pair a b)
lt2lp = fhl.map t2p

tl2pl :: ([a],[b]) -> Pair (List a) (List b)
tl2pl (xs,ys) = Pair (fhl xs) (fhl ys)

tests = [
        testGroup "Name" [
          testCase "name" (name @?= "TP06")
        ],
        testGroup "Show" [
          testProperty "show list" ((\xs -> show xs == (show.fhl) xs)::[Int] -> Bool),
          testProperty "show list of lists" ((\xs -> show xs == (show.fhl) (map fhl xs))::[[Int]] -> Bool),
          testProperty "show pair" ((\a b -> show (a,b) == show (Pair a b))::Int -> Char -> Bool)
        ],
        testGroup "Folds" [
          testProperty "foldr' id" ((\xs -> xs == wrap id xs)::[Int] -> Bool),
          testProperty "foldr' (+)" (xsCheck sum (foldr' (+) 0)::[Int] -> Bool),
          testProperty "foldl' (+)" (xsCheck sum (foldl' (+) 0)::[Int] -> Bool),
          testProperty "foldl' reverse" (xsCheck reverse (foldl' (flip (:)) [])::[Int] -> Bool),
          testProperty "foldl1' max" ((\xs -> xs /= [] ==> xsCheck maximum (foldl1' max) xs)::[Int] -> Property),
          testProperty "foldr1' max" ((\xs -> xs /= [] ==> xsCheck maximum (foldr1' max) xs)::[Int] -> Property),
          testProperty "foldl1' const" ((\xs -> xs /= [] ==> xsCheck head (foldl1' const) xs)::[Int] -> Property),
          testProperty "foldr1' const" ((\xs -> xs /= [] ==> xsCheck head (foldr1' const) xs)::[Int] -> Property),
          testProperty "foldl1' flip const" ((\xs -> xs /= [] ==> xsCheck last (foldl1' (\ _ x -> x)) xs)::[Int] -> Property),
          testProperty "foldr1' flip const" ((\xs -> xs /= [] ==> xsCheck last (foldr1' (\ _ x -> x)) xs)::[Int] -> Property),
          testCase "foldl1 Empty" (assertRaises (foldl1' max Empty::List Int) "empty list"),
          testCase "foldr1 Empty" (assertRaises (foldr1' max Empty::List Int) "empty list")
        ],
        testGroup "Lists" [
          testProperty "length'" (xsCheck length length'::[Int] -> Bool),
          testCase "null length" (length' Empty @?= 0),
          testProperty "head'" ((\xs -> xs /= [] ==> xsCheck head head' xs)::[Int] -> Property),
          testProperty "tail'" ((\xs -> xs /= [] ==> xsWrap tail tail' xs)::[Int] -> Property),
          testProperty "last'" ((\xs -> xs /= [] ==> xsCheck last last' xs)::[Int] -> Property),
          testProperty "init'" ((\xs -> xs /= [] ==> init xs == wrap init' xs)::[Int] -> Property),
          testProperty "null'" (xsCheck null null'::[Int] -> Bool),
          testProperty "at chars"((\n xs -> n >= 0 && n < length xs ==> xsCheck (!!n) (`at`n) xs)::Int -> String -> Property),
          testProperty "+++" ((\xs ys -> xsWrap (++ ys) (+++ fhl ys) xs):: [Int] -> [Int] -> Bool),
          testProperty "take'" ((\xs n -> xsWrap (take n) (take' n) xs)::String -> Int -> Bool),
          testProperty "drop'" ((\xs n -> xsWrap (drop n) (drop' n) xs)::String -> Int -> Bool),
          testProperty "int sum'" ((\xs -> sum xs == (sum'.fhl) xs)::[Int] -> Bool),
          testProperty "int product'" ((\xs -> product xs == (product'.fhl) xs)::[Int] -> Bool),
          testCase "zero product" ((product'.fhl) [3.1,2.3,5.0, 0.0] @?= 0.0),
          testProperty "reverse'" ((\xs -> reverse xs == wrap reverse' xs)::String -> Bool),
          testProperty "maximum'" ((\xs -> xs /= [] ==> maximum xs == (maximum'.fhl) xs)::[Int] -> Property),
          testProperty "minimum'" ((\xs -> xs /= [] ==> minimum xs == (minimum'.fhl) xs)::[Int] -> Property),
          testCase "maximum' empty list" (assertRaises (maximum' Empty::List Int) "empty list"),
          testCase "minimum' empty list" (assertRaises (minimum' Empty::List Int) "empty list"),
          testProperty "repeat'" ((\x -> replicate 25 x == take 25 ((fl.repeat') x))::Char -> Bool),
          testProperty "cycle'" ((\xs -> xs /= [] ==> take 100 (cycle xs) == take 100 (wrap cycle' xs))::String -> Property),
          testProperty "replicate'" ((\n c -> replicate n c == (fl.replicate' n) c)::Int -> Char -> Bool),
          testProperty "map' odd" ((\xs ->  map odd xs == wrap (map' odd) xs)::[Int] -> Bool),
          testProperty "map' (*3)" ((\xs ->  map (*3) xs == wrap (map' (*3)) xs)::[Int] -> Bool),
          testProperty "map' length" ((\xs ->  map length xs == wrap (map' length) xs)::[String] -> Bool),
          testProperty "filter' odd" ((\xs ->  filter odd xs == wrap (filter' odd) xs)::[Int] -> Bool),
          testProperty "filter' (>3)" ((\xs ->  filter (>3) xs == wrap (filter' (>3)) xs)::[Int] -> Bool),
          testProperty "filter' length" ((\xs ->  filter ((>3).length) xs == wrap (filter' ((>3).length))  xs)::[String] -> Bool),
          testProperty "concat'" ((\xs -> concat xs == wrap concat'  (map fhl xs))::[String] -> Bool),
          testProperty "and'" ((\xs -> and xs == (and'.fhl) xs)::[Bool] -> Bool),
          testProperty "or'" ((\xs -> or xs == (or'.fhl) xs)::[Bool] -> Bool),
          testCase "and' infinite list" ((and'.map' (/=9).fhl $ [1..]) @?= False),
          testCase "or' infinite list" ((or'.map' (==11).fhl $ [1..]) @?= True),
          testProperty "any' Bool" ((\xs -> or xs == (any' id . fhl) xs)::[Bool] -> Bool),
          testProperty "any' Int" ((\xs -> any odd xs == (any' odd . fhl) xs)::[Int] -> Bool),
          testProperty "any' Char" ((\xs -> let letter = (`elem`['a'..'z']) in any letter xs == (any' letter . fhl)  xs)::String -> Bool),
          testProperty "any' infinite list" ((\x -> x > 0 ==> any' (==x).fhl $ [1..])::Int -> Property),
          testProperty "all' Bool" ((\xs -> and xs == (all' id . fhl)  xs)::[Bool] -> Bool),
          testProperty "all' Int" ((\xs -> all odd xs == (all' odd . fhl)  xs)::[Int] -> Bool),
          testProperty "all' Char" ((\xs -> let letter = (`elem`['a'..'z']) in  all letter xs == (all' letter .fhl)  xs)::String -> Bool),
          testProperty "all' infinite list" ((\x -> x > 0 ==> not (all' (/=x). fhl $ [1..]))::Int -> Property),
          testProperty "elem' chars"((\c xs -> elem c xs  ==> elem' c . fhl $ xs)::Char -> String -> Property),
          testProperty "elem' chars"((\c xs -> notElem c xs  ==> not.elem' c .fhl $ xs)::Char -> String -> Property),
          testProperty "elem' infinite list" ((\x -> x > 0 ==> elem' x . fhl $ [1..])::Int -> Property),
          testProperty "notElem' chars"((\c xs -> elem c xs  ==> not.notElem' c . fhl $ xs)::Char -> String -> Property),
          testProperty "notElem' chars"((\c xs -> notElem c xs  ==> notElem' c . fhl $ xs)::Char -> String -> Property),
          testProperty "find' chars"((\c xs -> elem c xs  ==> (find' (==c).fhl) xs == Just c)::Char -> String -> Property),
          testProperty "find' chars"((\c xs -> notElem c xs  ==> isNothing((find' (==c).fhl) xs ))::Char -> String -> Property),
          testProperty "find' Int" ((\xs -> find odd xs == (find' odd .fhl) xs)::[Int] -> Bool),
          testProperty "find' infinite list" ((\x -> x >= 0 ==> (find' (>x) . fhl) [1..] == Just (x+1)):: Int -> Property),
          testProperty "zipWith' (+)" (xsysWrap (zipWith (+)) (zipWith' (+))::[Int] -> [Int] -> Bool),
          testProperty "zipWith' (++)" (xsysWrap (zipWith (++)) (zipWith' (++))::[String] -> [String] -> Bool),
          testProperty "zipWith' (==)" (xsysWrap (zipWith (==)) (zipWith' (==))::[Int] -> [Int] -> Bool),
          testProperty "takeWhile' odd" (xsWrap (takeWhile odd) (takeWhile' odd)::[Int] -> Bool),
          testProperty "takeWhile' (>0)" (xsWrap (takeWhile (>0)) (takeWhile' (>0))::[Int] -> Bool),
          testProperty "takeWhile' length" (xsWrap (takeWhile ((>3).length)) (takeWhile' ((>3).length))::[String] -> Bool),
          testProperty "dropWhile' odd" (xsWrap (dropWhile odd) (dropWhile' odd)::[Int] -> Bool),
          testProperty "dropWhile' (>0)" (xsWrap (dropWhile (>0)) (dropWhile' (>0))::[Int] -> Bool),
          testProperty "dropWhile' length" (xsWrap (dropWhile ((>3).length)) (dropWhile' ((>3).length))::[String] -> Bool),
          testProperty "iterate' (*(-2))" ((\x -> take 10 (iterate (*(-2)) x) == (take 10.fl) (iterate' (*(-2)) x))::Int -> Bool),
          testProperty "iterate' (x:)" ((\x -> take 10 (iterate (x:) []) == (take 10.fl) (iterate' (x:) []))::Char -> Bool)
          ],
        testGroup "Pairs" [
          testProperty "fst'" ((\x y -> fst' (Pair x y) == x)::Char -> Int -> Bool),
          testProperty "snd'" ((\x y -> snd' (Pair x y) == y)::Char -> Int -> Bool),
          testProperty "zip'" ((\xs ys -> zipWith (curry t2p) xs ys == fl (zip' (fhl xs) (fhl ys)))::[Int] -> String -> Bool),
          testProperty "unzip'" (prop_unzip::[(Int,Char)] -> Bool),
          testProperty "span' odd" ((\xs -> tl2pl (span odd xs) == span' odd (fhl xs))::[Int] -> Bool),
          testProperty "span' (>0)" ((\xs -> tl2pl (span (>0) xs) == span' (>0) (fhl xs))::[Int] -> Bool),
          testProperty "span' length" ((\xs ->  tl2pl (span ((>3).length) xs) == span' ((>3).length) (fhl xs))::[String] -> Bool),
          testProperty "partition' odd" ((\xs ->  tl2pl (partition odd xs) == partition' odd (fhl xs))::[Int] -> Bool),
          testProperty "partition' (>0)" ((\xs ->  tl2pl (partition (>0) xs) == partition' (>0) (fhl xs))::[Int] -> Bool),
          testProperty "partition' length" ((\xs ->  tl2pl (partition ((>3).length) xs) == partition' ((>3).length) (fhl xs))::[String] -> Bool),
          testProperty "lookup' Just k" ((\kvs k -> let v = lookup k kvs in isJust v  ==> lookup' k (lt2lp kvs) ==  v)::[(Char,Int)] -> Char -> Property),
          testProperty "lookup' Nothing" ((\kvs k -> isNothing (lookup k kvs) ==> isNothing( lookup' k (lt2lp kvs)))::[(Char,Int)] -> Char -> Property)
          ]
        ]


prop_unzip xys = (xs,ys) == (xs',ys') where
    (xs,ys) = unzip xys
    Pair xs'' ys'' = (unzip'.lt2lp) xys
    xs' = fl xs''
    ys' = fl ys''
