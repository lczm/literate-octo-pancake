import Data.Char (isDigit)
import Data.List (find,sort,elemIndex)
import Data.Maybe (maybe)

-- utils

findIndices' :: Int -> (a -> Bool) -> [a] -> [Int]
findIndices' s p = map fst . filter (p . snd) . zip [s..]

group :: Int -> [a] -> [[a]]
group n = map (take n) . takeWhile (not . null) . iterate (drop n)

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = xs

-- main

data Packet = PacketInt Int | PacketList [Packet]
    deriving (Show)

instance Eq Packet where
    (==)   (PacketInt a)    (PacketInt b)  = (==) a b
    (==) a@(PacketInt _)  b@(PacketList _) = (==) (PacketList [a]) b
    (==) a@(PacketList _) b@(PacketInt _)  = (==) a (PacketList [b])
    (==)   (PacketList a)   (PacketList b) = (==) a b

instance Ord Packet where
    compare   (PacketInt a)    (PacketInt b)  = compare a b
    compare a@(PacketInt _)  b@(PacketList _) = compare (PacketList [a]) b
    compare a@(PacketList _) b@(PacketInt _)  = compare a (PacketList [b])
    compare   (PacketList a)   (PacketList b) = compare a b

solve1 :: [(Packet,Packet)] -> Int
solve1 = sum . findIndices'' (uncurry (<=))
    where findIndices'' = findIndices' 1

solve2 :: [Packet] -> Int
solve2 ps = 
    let p1 = PacketList [PacketList [PacketInt 2]]
        p2 = PacketList [PacketList [PacketInt 6]]
        elemIndex' a = maybe undefined (+ 1) . elemIndex a
        ps1 = [p1,p2] ++ ps
        ps2 = sort ps1
        i1 = elemIndex' p1 ps2
        i2 = elemIndex' p2 ps2
    in  i1 * i2

parse1 :: String -> [(Packet,Packet)]
parse1 = map tup . group 2 . parse2
    where tup :: [a] -> (a,a)
          tup [x1,x2] = (x1,x2)
          tup _       = undefined

parse2 :: String -> [Packet]
parse2 = map parsePacket . filter (/= "") . lines
    where parsePacket :: String -> Packet
          parsePacket = fst . parsePacket'
              where parsePacket' :: String -> (Packet,String)
                    parsePacket' [] = undefined
                    parsePacket' cs@(c:cs')
                        | c == '[' = 
                            let css1 = iterate (removeComma . snd . parsePacket') cs'
                                cs'' = tail $ find' ((== ']') . head) css1
                                css2 = takeWhile ((/= ']') . head) css1
                                ps' = map (fst . parsePacket') css2
                            in  (PacketList ps',cs'')
                        | otherwise = 
                            let (a,b) = span isDigit cs
                            in  (PacketInt (read a),b)
                        where removeComma xs@(x:xs') = if x == ',' then xs' else xs
                              removeComma []         = undefined
                              find' p = maybe undefined id . find p

solution1 :: String -> String
solution1 = show . solve1 . parse1

solution2 :: String -> String
solution2 = show . solve2 . parse2

main :: IO ()
main = getContents >>= (putStrLn . solution2)