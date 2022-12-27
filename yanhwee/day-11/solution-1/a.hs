import Data.Array (Array,listArray,(//),accum,indices,(!),accumArray,bounds)
import Data.List (transpose,sortBy,foldl1')
import Data.Bool (bool)

-- utils

divisibleBy :: Integer -> Integer -> Bool
divisibleBy a b = rem b a == 0

zipWithScan :: (a -> b -> c) -> (b -> a -> b) -> b -> [a] -> [c]
zipWithScan f g a = (zipWith f) <*> (scanl g a)

twin :: (a -> a -> b) -> a -> b
twin f = f <*> id

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

-- module stack

data Queue a = Queue Int (Array Int a)

queue :: Int -> a -> [a] -> Queue a
queue n x xs = Queue (length xs) a
    where a = accumArray (const id) x (1,n) (zip [1..] xs)

push :: Queue a -> a -> Queue a
push q@(Queue i a) x = if i' > l then q else Queue i' a'
    where (_,l) = bounds a
          i' = i + 1
          a' = a // [(i',x)]

clear :: Queue a -> Queue a
clear (Queue _ a) = Queue 0 a

elems :: Queue a -> [a]
elems (Queue i a) = map (a !) [1..i]

-- main

data Monkey = Monkey {
    items :: Queue Integer,
    op :: (Integer -> Integer),
    throw :: (Integer -> Int)
}

solve :: [Monkey] -> Integer
solve = id
    .toInteger . product
    . take 2
    . sortDesc
    . foldl1' (zipWith (+))
    -- . map sum . transpose
    -- . drop 9900
    . take 10000
    . map (map length . throwsByMonkeys)
    -- . map throwsByMonkeys
    . iterate updateMonkeys
    . makeMonkeyArray

makeMonkeyArray :: [Monkey] -> Array Int Monkey
makeMonkeyArray [] = undefined
makeMonkeyArray ms = listArray (0, length ms - 1) ms

updateMonkeys :: Array Int Monkey -> Array Int Monkey
updateMonkeys ms = foldl updateIthMonkey ms (indices ms)

throwsByMonkeys :: Array Int Monkey -> [[(Int,Integer)]]
throwsByMonkeys ms = zipWithScan throwsByMonkey' updateIthMonkey ms (indices ms)
    where throwsByMonkey' i ms' = throwsByMonkey (ms' ! i)

updateIthMonkey :: Array Int Monkey -> Int -> Array Int Monkey
updateIthMonkey ms i = ms''
    where ixs = throwsByMonkey (ms ! i)
          ms' = clearIthMonkey ms i
          ms'' = giveMonkeys ms' ixs

clearIthMonkey :: Array Int Monkey -> Int -> Array Int Monkey
clearIthMonkey ms i = ms // [(i, clearMonkey (ms ! i))]

clearMonkey :: Monkey -> Monkey
clearMonkey m = m { items = clear (items m) }

giveMonkeys :: Array Int Monkey -> [(Int,Integer)] -> Array Int Monkey
giveMonkeys ms = accum giveMonkey ms

giveMonkey :: Monkey -> Integer -> Monkey
giveMonkey m a = m { items = push (items m) a }

throwsByMonkey :: Monkey -> [(Int,Integer)]
throwsByMonkey m = (((flip zip) <*> map (throw m)) . map ((`rem` p2) . (op m))) (elems $ items m)
    where p1 = 13 * 17 * 19 * 23
          p2 = 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19
-- throwsByMonkey m = (((flip zip) <*> map (throw m)) . map (op m)) (elems $ items m)
-- throwsByMonkey m = (((flip zip) <*> map (throw m)) . map ((`div` 3) . (op m))) (elems $ items m)

prepare :: [([Integer],(Integer -> Integer),(Integer -> Int))] -> [Monkey]
prepare xsots = map f xsots
    where n = sum $ map (length . get1) xsots
              where get1 = \(a,_,_) -> a
          f (a,b,c) = Monkey (queue n 0 a) b c

solution :: [([Integer],(Integer -> Integer),(Integer -> Int))] -> Integer
solution = solve . prepare

inputTest1 :: [([Integer],(Integer -> Integer),(Integer -> Int))]
inputTest1 = [
    ([79,98],(*19),(bool 3 2 . (divisibleBy 23))),
    ([54,65,75,74],(+6),(bool 0 2 . (divisibleBy 19))),
    ([79,60,97],(\x -> x * x),(bool 3 1 . (divisibleBy 13))),
    ([74],(+3),(bool 1 0 . (divisibleBy 17)))]

inputIn1 :: [([Integer],(Integer -> Integer),(Integer -> Int))]
inputIn1 = [
    ([99, 67, 92, 61, 83, 64, 98],(* 17),(bool 2 4 . divisibleBy 3)),
    ([78, 74, 88, 89, 50],(* 11),(bool 5 3 . divisibleBy 5)),
    ([98, 91],(+ 4),(bool 4 6 . divisibleBy 2)),
    ([59, 72, 94, 91, 79, 88, 94, 51],(twin (*)),(bool 5 0 . divisibleBy 13)),
    ([95, 72, 78],(+ 7),(bool 6 7 . divisibleBy 11)),
    ([76],(+ 8),(bool 2 0 . divisibleBy 17)),
    ([69, 60, 53, 89, 71, 88],(+ 5),(bool 1 7 . divisibleBy 19)),
    ([72, 54, 63, 80],(+ 3),(bool 3 1 . divisibleBy 7))]

main :: IO ()
main = (print . solution) inputIn1