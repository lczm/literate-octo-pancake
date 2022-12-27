import Data.List.NonEmpty ()
import Data.Bool (bool)

-- utils

last' :: a -> [a] -> a
last' = foldl (const id)

zipWithScan :: (a -> b -> c) -> (b -> a -> b) -> b -> [a] -> [c]
zipWithScan f g a = (zipWith f) <*> (scanl g a)

mapWhileIterate :: (a -> b) -> (a -> Bool) -> (a -> a) -> a -> [b]
mapWhileIterate f p g = map f . takeWhile p . iterate g

group :: Int -> [a] -> [[a]]
group n = mapWhileIterate (take n) (not . null) (drop n)

-- main

data Op = Noop | Addx Int

parse :: String -> [Op]
parse = map parseLine . lines
    where parseLine text
            | a == "noop" = Noop
            | a == "addx" = Addx (read b)
            | otherwise   = undefined
            where ws = words text
                  a = ws !! 0
                  b = ws !! 1

solve1 :: (Int,Int) -> [Int] -> [Op] -> Int
solve1 (i1,x1) steps = id 
    . sum 
    . zipWith (*) steps 
    . map snd
    . takeBefores (\ix (ix',_)-> ix' <= ix) steps (i1,x1)
    . runAndIndex (i1,x1)
    where runAndIndex = scanl update
              where update :: (Int,Int) -> Op -> (Int,Int)
                    update (i,x) Noop     = (i+1,x)
                    update (i,x) (Addx n) = (i+2,x+n)
          splitWith p ds xs = zipWithScan take' drop' xs ds
              where drop' xs' d = dropWhile (p d) xs'
                    take' d xs' = takeWhile (p d) xs'
          takeBefores p ds x xs = tail $ scanl last' x $ splitWith p ds xs

unsparsify :: Int -> (Int,a) -> [(Int,a)] -> [a]
unsparsify j ix ixs = concatMap (uncurry replicate) nxs
    where ixs' = ix:ixs
          (is,xs) = unzip ixs'
          ns = zipWith (-) (tail is ++ [j]) (is)
          nxs = zip ns xs

solve2 :: [Op] -> String
solve2 = id
    . unlines
    . (map . map) (bool '.' '#')
    . (zipWith . zipWith) inPos (replicate h [0,1..w-1])
    . group w
    . unsparsify (w * h) (i1,x1)
    . runAndIndex (i1,x1)
    where (i1,x1) = (1,1)
          (w,h) = (40,6)
          inPos i x = x - 1 <= i && i <= x + 1
          runAndIndex = scanl update
              where update :: (Int,Int) -> Op -> (Int,Int)
                    update (i,x) Noop     = (i+1,x)
                    update (i,x) (Addx n) = (i+2,x+n)

solution1 :: String -> Int
solution1 = solve1 (i1,x1) steps. parse
    where (i1,x1) = (1,1)
          (start,step,stop) = (20,40,220)
          steps = [start,start+step..stop]

solution2 :: String -> String
solution2 = solve2 . parse

main :: IO ()
main = getContents >>= (putStr . solution2)