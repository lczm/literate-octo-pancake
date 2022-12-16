testRangePair :: Ord a => ((a,a),(a,a)) -> Bool
testRangePair ((a1,a2),(b1,b2)) =
    a1 == b1 || a2 == b2 || (a1 < b1) /= (a2 < b2)

testRangePair2 :: Ord a => ((a,a),(a,a)) -> Bool 
testRangePair2 ((a1,a2),(b1,b2)) =
    not $ (a2 < b1) || (b2 < a1)

solve1 :: Ord a => [((a,a),(a,a))] -> Int
solve1 = length . filter testRangePair

solve2 :: Ord a => [((a,a),(a,a))] -> Int
solve2 = length . filter testRangePair2

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = xs

span' :: (a -> Bool) -> [a] -> ([a],[a])
span' p xs = (takeWhile p xs, dropWhile' p xs)

splitOnce :: Eq a => a -> [a] -> ([a],[a])
splitOnce x = span' (/= x)

parseRange :: String -> (Int,Int)
parseRange s = 
    let (a,b) = splitOnce '-' s
    in (read a,read b)

parseLine :: String -> ((Int,Int),(Int,Int))
parseLine s =
    let (a,b) = splitOnce ',' s
    in (parseRange a, parseRange b)

parse :: String -> [((Int,Int),(Int,Int))]
parse = map parseLine . lines

solution1 :: String -> Int
solution1 = solve1 . parse

solution2 :: String -> Int
solution2 = solve2 . parse

debug :: String -> String 
debug = show . lines

main :: IO ()
main = getContents >>= (print . solution2)
