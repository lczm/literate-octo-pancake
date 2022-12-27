{-# LANGUAGE TupleSections #-}

import Data.Array (Ix,Array,(!),indices,(//),listArray,bounds,range)
import Data.Maybe (fromJust)
import Data.Function (on)
import Data.Bool (bool)

-- utils

arrElemIndices :: Ix i => (e -> Bool) -> Array i e -> [i]
arrElemIndices p a = filter (p . (a !)) (indices a)

arrElemIndex :: Ix i => (e -> Bool) -> Array i e -> Maybe i
arrElemIndex p a = if null is then Nothing else Just (head is)
    where is = arrElemIndices p a

-- main

make2DArray :: [[a]] -> Array (Int,Int) a
make2DArray xss = listArray ((1,1),(i,j)) (concat xss)
    where i = length xss
          j = length $ head xss

prepare :: [[Char]] -> ((Int,Int), (Int,Int), Array (Int,Int) Char)
prepare heights' = (start,end,heightArray)
    where heightArray' = make2DArray heights'
          start = fromJust $ arrElemIndex (== 'S') heightArray'
          end = fromJust $ arrElemIndex (== 'E') heightArray'
          heightArray = heightArray' // [(start,'a'),(end,'z')]

stepsIndex2DBounded :: (Ix i, Ix j, Enum i, Enum j) => ((i,j),(i,j)) -> (i,j) -> [(i,j)]
stepsIndex2DBounded ((i1,j1),(i2,j2)) (i,j) = map snd $ filter fst $ bijs
    where bijs = [
              (pred i >= i1,(pred i,j)),
              (succ i <= i2,(succ i,j)),
              (pred j >= j1,(i,pred j)),
              (succ j <= j2,(i,succ j))]

stepsIndex2DArray :: (Ix i, Enum i, Ix j, Enum j) => (a -> a -> Bool) -> Array (i,j) a -> (i,j) -> [(i,j)]
stepsIndex2DArray c a i = filter (c' i) $ (stepsIndex2DBounded (bounds a) i)
    where c' = c `on` (a !)

-- prettyArray :: Array (Int,Int) Char -> String
-- prettyArray a = ss
--     where ((i1,j1),(i2,j2)) = bounds a
--           ijs = [[(i,j) | j <- range (j1,j2)] | i <- range (i1,i2)]
--           css = map (map (a !)) ijs
--           ss = unlines css

-- $ unlines $ map prettyArray
-- $ map (fmap (bool '.' '#') . fst)

solve1 :: [[Char]] -> Int
solve1 heights' = id
    $ length
    $ takeWhile (not . elem end . snd)
    $ iterate search (visits // [(start,True)], [start])
    where (start,end,heights) = prepare heights'
          visits = fmap (const False) heights
          step = stepsIndex2DArray (\a b -> b <= succ a) heights
          search (bs,is) = (bs1,is2)
              where is1 = concatMap step is
                    bss1 = scanl setTrue bs is1
                        where setTrue arr i = arr // [(i,True)]
                    is2 = map snd $ filter (uncurry elemFalse) (zip bss1 is1)
                        where elemFalse arr i = not $ arr ! i
                    bs1 = last bss1

solve2 :: [[Char]] -> Int
solve2 heights' = id
    $ length
    $ takeWhile (not . any ((== 'a') . (heights !)) . snd)
    $ iterate search (visits // [(end,True)], [end])
    where (_,end,heights) = prepare heights'
          visits = fmap (const False) heights
          step = stepsIndex2DArray (\a b -> a <= succ b) heights
          search (bs,is) = (bs1,is2)
              where is1 = concatMap step is
                    bss1 = scanl setTrue bs is1
                        where setTrue arr i = arr // [(i,True)]
                    is2 = map snd $ filter (uncurry elemFalse) (zip bss1 is1)
                        where elemFalse arr i = not $ arr ! i
                    bs1 = last bss1

parse :: String -> [[Char]]
parse = lines

solution1 :: String -> String
solution1 = show . solve1 . parse

solution2 :: String -> String
solution2 = show . solve2 . parse

main :: IO ()
main = getContents >>= (putStrLn . solution2)