import Data.List (sortBy, groupBy, find)
import Data.Ord (comparing)
import Data.Function (on)

data Tree a = Tree a [Tree a]

instance Functor Tree where
    fmap f (Tree a ts) = Tree (f a) (map (fmap f) ts)
    (<$) a (Tree _ ts) = Tree a (map ((<$) a) ts)

unfoldTree :: (b -> (a,[b])) -> b -> Tree a
unfoldTree f b = Tree a ts
    where (a,bs) = f b
          ts = map (unfoldTree f) bs

value :: Tree a -> a
value (Tree a _) = a

subtrees :: Tree a -> [Tree a]
subtrees (Tree _ ts) = ts

scanTree :: (a -> [a] -> a) -> Tree a -> Tree a
scanTree f (Tree a ts) = Tree (f a (map value ts')) ts'
    where ts' = map (scanTree f) ts

flatten :: Tree a -> [a]
flatten (Tree a ts) = a : concatMap flatten ts

solve1 :: Int -> Tree [Int] -> Int
solve1 n = sum . filter (< n) . flatten . scanTree sum' . fmap sum
    where sum' x xs = sum (x:xs)

solve2 :: Int -> Tree [Int] -> Int
solve2 n t = last $ takeWhile (> d) sizes
    where sum' x xs = sum (x:xs)
          sortDesc = sortBy (flip compare)
          sizes = (sortDesc . flatten . scanTree sum' . fmap sum) t
          m = head sizes
          d = m - n

data Thing = CD String | LS | File Int String | Dir String

data State = State {
    path :: [String],
    files :: [(Int,String)],
    dirs :: [String]
}

parseThing :: String -> Thing
parseThing text
    | a == "$" && b == "cd" = CD c
    | a == "$" && b == "ls" = LS
    | a == "dir"            = Dir b
    | otherwise             = File (read a) b
    where ws = words text
          a = ws !! 0
          b = ws !! 1
          c = ws !! 2

step :: State -> Thing -> State
step state thing = State path'' files'' dirs''
    where path' = path state
          files' = files state
          dirs' = dirs state
          (path'',files'',dirs'') = case thing of
            (CD name)        -> case name of 
                "/"   -> ([],[],[])
                ".."  -> (tail path',[],[])
                name' -> (name':path',[],[])
            (LS)             -> (path',[],[])
            (File size name) -> (path',(size,name):files',dirs')
            (Dir name)       -> (path',files',name:dirs')

steps :: State -> [Thing] -> [State]
steps s = scanl step s

sortAndGroupBy :: (Ord b) => (a -> b) -> [a] -> [[a]]
sortAndGroupBy f = groupBy (equalOn f) . sortBy (comparing f)
    where equalOn = on (==)

pruneStates :: [State] -> [State]
pruneStates = map (head . sortFileDirLengths) . groupPaths
    where groupPaths = sortAndGroupBy path
          fileDirLength s = length (files s) + length (dirs s)
          sortFileDirLengths = sortBy (flip (comparing fileDirLength))

makeTree :: [State] -> Tree ([String],[(Int,String)])
makeTree states = unfoldTree make []
    where make path' = maybe undefined make' state
              where state = find ((== path') . path) states
                    make' state' = ((path', files state'), map (: path') (dirs state'))

prepareTree :: Tree ([String],[(Int,String)]) -> Tree [Int]
prepareTree = fmap (map fst . snd)

parse :: String -> Tree [Int]
parse = prepareTree . makeTree . pruneStates . steps rootState . map parseThing . lines
    where rootState = State [] [] []

solution1 :: String -> Int
solution1 = solve1 100000 . parse

solution2 :: String -> Int
solution2 = solve2 maxUsed . parse
    where available = 70000000
          need = 30000000
          maxUsed = available - need

main :: IO ()
main = getContents >>= (print . solution2)