import Data.Set (fromList)
import Data.List (transpose)

-- utils

countUniques :: Ord a => [a] -> Int 
countUniques = length . fromList

-- main

data Direction = North | South | East | West
type Motion = (Direction,Int)
type Position = (Int,Int)

solve :: Int -> Position -> [Motion] -> Int 
solve n p1 = id
    . countUniques
    . (!! n)
    . iterate updates
    . updates' p1
    . concatMap (uncurry (flip replicate))

update :: Position -> Position -> Position
update (x1,y1) (x2,y2) =
    let dx = x2 - x1
        dy = y2 - y1
        out = max (abs dx) (abs dy) > 1
        go d u = u + if out then signum d else 0
        x3 = go dx x1
        y3 = go dy y1
    in  (x3,y3)

updates :: [Position] -> [Position]
updates = scanl1 update

update' :: Position -> Direction -> Position
update' (x,y) North = (x,y+1)
update' (x,y) South = (x,y-1)
update' (x,y) East = (x+1,y)
update' (x,y) West = (x-1,y)

updates' :: Position -> [Direction] -> [Position]
updates' = scanl update'

parse :: String -> [Motion]
parse = map parseLine . lines
    where parseLine text
              | a == "L" = (West,n)
              | a == "R" = (East,n)
              | a == "U" = (North,n)
              | a == "D" = (South,n)
              | otherwise = undefined
              where ws = words text
                    a = ws !! 0
                    b = ws !! 1
                    n = read b

solution :: String -> String
solution = show . solve n origin . parse
    where origin = (0,0)
          n = 9

main :: IO ()
main = getContents >>= (putStrLn . solution)

