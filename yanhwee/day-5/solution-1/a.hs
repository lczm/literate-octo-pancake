import Data.Array (Array, listArray, (!), (//), bounds, elems)
import Data.List (transpose)

-- utils
fillTill :: Int -> a -> [a] -> [a]
fillTill n a xs = xs ++ ys
    where m = n - length xs
          ys = replicate m a

every :: Int -> [a] -> [a]
every _ [] = []
every n xs@(x:_) = x : (every n $ drop n xs)

-- module Vector
data Vector a = Vector (Array Int a)
    deriving (Show)

vector :: [a] -> Vector a
vector xs = Vector $ listArray (1, length xs) xs

size :: Vector a -> Int
size (Vector array) = snd $ bounds array

get :: Vector a -> Int -> a
get (Vector array) = (array !)

set :: Vector a -> [(Int, a)] -> Vector a
set (Vector array) = Vector . (array //)

elements :: Vector a -> [a]
elements (Vector a) = elems a

-- module Stack
data Stack a = Stack Int (Vector a)
    deriving (Show)

stack :: Int -> a -> [a] -> Stack a
stack n a xs = Stack (length xs) (vector (fillTill n a xs))

push :: Stack a -> [a] -> Stack a
push (Stack i v) xs = 
    let i' = min (size v) (i + length xs)
        v' = set v (zip [i+1,i+2..i'] xs)
    in Stack i' v'

pop :: Stack a -> Int -> (Stack a, [a])
pop (Stack i v) n =
    let i' = max 0 (i - n)
        xs = map (get v) [i,i-1..i'+1]
    in (Stack i' v, xs)

popr :: Stack a -> Int -> (Stack a, [a])
popr (Stack i v) n =
    let i' = max 0 (i - n)
        xs = map (get v) [i'+1,i'+2..i]
    in (Stack i' v, xs)

peek1 :: Stack a -> a
peek1 = head . snd . pop' 1
    where pop' = flip pop

-- main
update :: Vector (Stack a) -> (Int,Int,Int) -> Vector (Stack a)
update v (n,a,b) = v'
    where s1 = get v a
          s2 = get v b
          (s1',xs) = pop s1 n
          s2' = push s2 xs
          v' = set v [(a,s1'),(b,s2')]

update' :: Vector (Stack a) -> (Int,Int,Int) -> Vector (Stack a)
update' v (n,a,b) = v'
    where s1 = get v a
          s2 = get v b
          (s1',xs) = popr s1 n
          s2' = push s2 xs
          v' = set v [(a,s1'),(b,s2')]

prepare :: [[Char]] -> Vector (Stack Char)
prepare xss = vector $ map stack' xss
    where m = sum $ map length xss
          stack' = stack m 'a'

solve1 :: [[Char]] -> [(Int,Int,Int)] -> String
solve1 xss = map peek1 . elements . foldl update (prepare xss)

solve2 :: [[Char]] -> [(Int,Int,Int)] -> String
solve2 xss = map peek1 . elements . foldl update' (prepare xss)

parse :: String -> ([[Char]],[(Int,Int,Int)])
parse text =
    let (a,b) = span (/= "") $ lines text
        b' = tail b
    in (parseDrawing a, parseInstructions b')

parseDrawing :: [String] -> [[Char]]
parseDrawing = map (takeWhile (/= ' ')) . transpose . map (every 4 . tail) . reverse . init

parseInstructions :: [String] -> [(Int,Int,Int)]
parseInstructions = 
    let parseLine = \s -> 
            let ws = words s
                a = read $ ws !! 1
                b = read $ ws !! 3
                c = read $ ws !! 5
            in (a,b,c)
    in map parseLine

solution1 :: String -> String
solution1 = solve' . parse
    where solve' = uncurry solve1

solution2 :: String -> String
solution2 = solve' . parse
    where solve' = uncurry solve2

debug :: String -> String
debug text =
    let (a,b) = parse text
    in show $ elements $ foldl update (prepare a) b

main :: IO ()
main = getContents >>= (print . solution2)