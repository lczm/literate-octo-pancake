{-# LANGUAGE TupleSections #-}

import Data.Array (Array, Ix, (!), bounds, listArray, elems, range)
import Data.List (transpose, zipWith4)
import Data.Char (digitToInt)

twin :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
twin x y z = (x . y) <*> z

twin' :: (a -> a -> b) -> a -> b
twin' f = twin f id id

monostack :: (a -> a -> Bool) -> (b -> [c] -> c) -> [(a,b)] -> [c]
monostack f g = (zipWith get) <*> (scanl set' [])
    where get (a,b) acs = g b $ map snd $ takeWhile (f a . fst) acs
          set (a,b) acs = (a, get (a,b) acs) : dropWhile (f a . fst) acs
          set' = flip set

monostack1 :: (a -> a -> Bool) -> ([b] -> b) -> [a] -> [b]
monostack1 f g = monostack f (const g) . map (,())

monostack2 :: (a -> a -> Bool) -> (a -> [b] -> b) -> [a] -> [b]
monostack2 f g = monostack f g . map (twin' (,))

accLocalLT :: [Int] -> [Int]
accLocalLT = monostack1 (>) ((+ 1) . sum)

cross :: [a] -> [b] -> [[(a,b)]]
cross xs ys = map' xs (map' ys . (,))
    where map' = flip map

cross' :: [a] -> [b] -> [[(a,b)]]
cross' xs ys = map' ys (map' xs . flip (,))
    where map' = flip map

quadply :: (Ix i, Ix j) => ([a] -> [b]) -> Array (i,j) a -> Array (i,j) (b,b,b,b)
quadply f a = b
    where ((i1,j1),(i2,j2)) = bounds a
          is = range (i1,i2)
          js = range (j1,j2)
          is' = reverse is
          js' = reverse js
          ijs = cross is js
          ijs' = cross is js'
          jis = cross' is js
          jis' = cross' is' js
          index = map (map (a !))
          drxs = index ijs
          dlxs = index ijs'
          rdxs = index jis
          ruxs = index jis'
          drys = map f drxs
          dlys = map f dlxs
          rdys = map f rdxs
          ruys = map f ruxs
          reverse' = map reverse
          ys1 = drys
          ys2 = reverse' dlys
          ys3 = transpose rdys
          ys4 = (transpose . reverse') ruys
          merge = zipWith4 (zipWith4 (,,,))
          zs = merge ys1 ys2 ys3 ys4
          b = listArray (bounds a) (concat zs)

subarray :: (Ix i) => (i,i) -> Array i e -> Array i e
subarray b1 a = listArray b1 (map (a !) (range b1))

solve :: Array (Int,Int) Int -> Int
solve = maximum . map mul4 . elems . quadply accLocalLT . shrink1
    where mul4 (a,b,c,d) = a * b * c * d
          shrink1 a = subarray ((i1+1,j1+1),(i2-1,j2-1)) a
              where ((i1,j1),(i2,j2)) = bounds a

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

prepare :: [[Int]] -> Array (Int,Int) Int
prepare xss = listArray ((1,1),(i,j)) (concat xss)
    where i = length xss
          j = length $ head xss

solution :: String -> Int
solution = solve . prepare . parse

main :: IO ()
main = getContents >>= (print . solution)