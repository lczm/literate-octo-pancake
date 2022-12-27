{-# LANGUAGE TupleSections #-}

import Data.Array (Ix,Array,(!),(//),inRange,accumArray,bounds)
import Data.List (find,unfoldr)
import Data.Maybe (listToMaybe)
import Data.Bool (bool)

-- utils

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs xs = (zip <*> tail) xs

filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe p a = listToMaybe (filter p [a])

unfoldr' :: (a -> Maybe a) -> a -> [a]
unfoldr' f = (:) <*> (unfoldr (fmap twin . f))
    where twin a = (a,a)

plainArray :: Ix i => (i,i) -> e -> Array i e
plainArray b e = accumArray undefined e b []

-- main

type Position = (Int,Int)

lineRange :: Position -> Position -> [Position]
lineRange (x1,y1) (x2,y2)
    | x1 == x2 = map (x1,) [(min y1 y2)..(max y1 y2)]
    | y1 == y2 = map (,y1) [(min x1 x2)..(max x1 x2)]
    | otherwise = undefined

setTrues :: (Ix i) => Array i Bool -> [i] -> Array i Bool
setTrues arr = (arr //) . map (,True)

setTrue :: (Ix i) => Array i Bool -> i -> Array i Bool
setTrue arr i = setTrues arr [i]

falls :: Position -> [Position]
falls (x,y) = map (,y+1) [x,x-1,x+1]

isAir :: Array Position Bool -> Position -> Bool
isAir bs p = not (bs ! p)

fall :: Array Position Bool -> Position -> Maybe (Maybe Position)
fall bs = id
    . find (maybe True (isAir bs))
    . map (filterMaybe (inRange (bounds bs)))
    . falls

simulate1 :: Position -> Array Position Bool -> Maybe (Array Position Bool)
simulate1 p bs = id
    $ bool' (isAir bs p) Nothing
    $ fmap (setTrue bs)
    $ last
    $ unfoldr' (>>= (fall bs))
    $ Just p
    where bool' b f t = bool f t b

simulate :: Position -> Array Position Bool -> [Array Position Bool]
simulate p = unfoldr' (simulate1 p)

prepare1 :: Position -> [[Position]] -> Array Position Bool
prepare1 p ls = 
    let ps = p : concat ls
        (xs,ys) = unzip ps
        (xmin,ymin) = (minimum xs, minimum ys)
        (xmax,ymax) = (maximum xs, maximum ys)
        bs = plainArray ((xmin,ymin),(xmax,ymax)) False
        is = concatMap (concatMap (uncurry lineRange) . pairs) ls
        bs' = setTrues bs is
    in  bs'

prepare2 :: Position -> [[Position]] -> Array Position Bool
prepare2 (x,y) ls = 
    let ps = (x,y) : concat ls
        (xs,ys) = unzip ps
        (xmin,ymin) = (minimum xs, minimum ys)
        (xmax,ymax) = (maximum xs, maximum ys)
        ymax' = ymax + 2
        xmin' = min xmin (x - ymax')
        xmax' = max xmax (x + ymax')
        ls' = [(xmin',ymax'),(xmax',ymax')] : ls
        bs = plainArray ((xmin',ymin),(xmax',ymax')) False
        is = concatMap (concatMap (uncurry lineRange) . pairs) ls'
        bs' = setTrues bs is
    in  bs'

solve1 :: Position -> [[Position]] -> Int
solve1 p ls =
    let bs = prepare1 p ls
    in  length $ tail $ simulate p bs

solve2 :: Position -> [[Position]] -> Int
solve2 p ls =
    let bs = prepare2 p ls
    in  length $ tail $ simulate p bs

parseLine :: String -> [Position]
parseLine = map (g . f) . map (break (== ',')) . filter (/= "->") . words
    where f (a,b) = (a, tail b)
          g (a,b) = (read a, read b)

parse :: String -> [[Position]]
parse = map parseLine . lines

solution1 :: String -> String
solution1 = show . solve1 p . parse
    where p = (500,0)

solution2 :: String -> String
solution2 = show . solve2 p . parse
    where p = (500,0)

main :: IO ()
main = getContents >>= (putStrLn . solution2)